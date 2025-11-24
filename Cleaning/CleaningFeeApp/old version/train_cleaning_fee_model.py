from pathlib import Path
import sys
import numpy as np
import pandas as pd
import joblib
#import matplotlib.pyplot as plt

from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import RepeatedKFold, GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.metrics import mean_squared_error
from sklearn.inspection import PartialDependenceDisplay

external_path = Path("/Users/ylin/My Drive/Cohost/Data and Reporting/Codes/python/")
sys.path.append(str(external_path))
from DataProcessing import import_data,property_input,format_reservation

def load_data() -> pd.DataFrame:
    """
    Load and merge property, cleaning, and bookings data.

    Assumptions (align with your R code):
    - data/Property_Cohost.xlsx has:
        * Sheet 0: property details (Listing, PropertyType, Region, OCCUPANCY, SqFt, BEDROOMS, BEDS, BATHROOMS)
        * Sheet "Cleaning": cleaning fees (Listing, Group, Cleaner.lead, Cleaning.fee)
    - data/bookings.csv OR data/bookings.xlsx has:
        * Listing
        * checkout_date
        * AvgDailyRate
        * guests
    """
    bookings = import_data()
    filepath = "/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/"
    property = pd.read_excel(filepath+"Property_Cohost.xlsx")
    cleaner = pd.read_excel(filepath+"Property_Cohost.xlsx",sheet_name="Cleaning")

    data = property[['Listing', 'PropertyType','Region','OCCUPANCY', 
        'SqFt', 'BEDROOMS', 'BEDS', 'BATHROOMS']].merge(cleaner[['Listing','Group',
        'Cleaner lead','Cleaning.fee']], on="Listing",how="outer")
    data = data.loc[(data["Cleaning.fee"].notna()) &(data["SqFt"].notna()) &(data["OCCUPANCY"].notna())]
    data["hottub"]=np.where(data["Listing"].isin(["Lilliwaup 28610", "Shelton 310", "Shelton 250", 
                                            "Hoodsport 26060", "Longbranch 6821", "Poulsbo 3956"]),"Yes","No")
    data["PropertyType"] = np.where(data["PropertyType"].isin(["Guest suite","Guesthouse"]),"Guesthouse_ADU",data["PropertyType"])

    Guests = (bookings[(bookings["checkout_date"]>=pd.to_datetime("2024-01-01"))&(bookings["checkout_date"]<=pd.to_datetime("2025-09-30"))]
    .groupby("Listing")
    .agg(avg_bookings_per_month=("Confirmation.Code","nunique"),
        avg_rate = ("AvgDailyRate","mean"),
        avg_rate_per_guest =("AvgDailyRate",
                lambda x: (x / bookings.loc[x.index, "guests"]).mean(),
            ),
        avg_guest = ("guests","mean"),
        med_guest =("guests","median")))
    Guests["avg_bookings_per_month"] = Guests["avg_bookings_per_month"] /21

    data = data.merge(Guests,on="Listing",how="left")
    mask = (data["Listing"].isin(["Cottages All OSBR"])) |(data["avg_bookings_per_month"].isna())
    data = data[~mask]
    return data

NUMERIC_FEATURES = [
    "OCCUPANCY",
    "SqFt",
    "BEDROOMS",
    "BEDS",
    "BATHROOMS",
    #"med_guest",
    #"avg_bookings_per_month",
    "avg_rate",
    #"avg_rate_per_guest",
]

CATEGORICAL_FEATURES = [
    "hottub",
    "PropertyType",
    "Region",
]

# -----------------------------
# MODEL BUILDING (Pipeline + RF)
# -----------------------------
def build_pipeline() -> Pipeline:
    """
    Build preprocessing + RF pipeline.
    """
    preprocess = ColumnTransformer(
        transformers=[
            ("num", "passthrough", NUMERIC_FEATURES),
            ("cat", OneHotEncoder(handle_unknown="ignore"), CATEGORICAL_FEATURES),
        ]
    )

    rf = RandomForestRegressor(
        n_estimators=500,
        random_state=123,
        n_jobs=-1,
    )

    model = Pipeline(
        steps=[
            ("preprocess", preprocess),
            ("rf", rf),
        ]
    )
    return model

def train_with_repeated_cv(df: pd.DataFrame,) -> tuple[Pipeline, float]:
    """
      - RepeatedKFold (5 folds, 3 repeats)
      - GridSearchCV over 5 values of mtry (max_features)
    """
    predictor_cols = NUMERIC_FEATURES + CATEGORICAL_FEATURES

    missing = [c for c in predictor_cols + ["Cleaning.fee"] if c not in df.columns]
    if missing:
        raise KeyError(f"Missing expected columns in data: {missing}")

    df_model = df.dropna(subset=predictor_cols + ["Cleaning.fee"]).copy()
    X = df_model[predictor_cols]
    y = df_model["Cleaning.fee"].astype(float)

    model = build_pipeline()

    # caret's tuneLength = 5  → 5 candidate values of mtry
    p = len(predictor_cols)
    mtry_grid = np.linspace(1, p, 5, dtype=int)

    param_grid = {
        "rf__max_features": mtry_grid,
    }

    cv = RepeatedKFold(
        n_splits=5,
        n_repeats=3,
        random_state=123,
    )

    grid = GridSearchCV(
        estimator=model,
        param_grid=param_grid,
        scoring="neg_root_mean_squared_error",
        cv=cv,
        n_jobs=-1,
        verbose=1,
    )

    grid.fit(X, y)

    best_model: Pipeline = grid.best_estimator_
    best_rmse = -grid.best_score_

    print("Best params (approx caret tuneLength grid):", grid.best_params_)
    print(f"Best CV RMSE: {best_rmse:.2f}")

    # quick holdout evaluation using same data just for sanity
    y_pred = best_model.predict(X)
    mse = mean_squared_error(y, y_pred)
    print(f"Training-set MSE (not CV): {mse:.2f}")

    return best_model, best_rmse


def get_aggregated_importances(
    model,
    numeric_features,
    categorical_features,
):
    """
    Compute feature importances aggregated back to original features
    (numeric + categorical), even when the model is a Pipeline with
    a ColumnTransformer + OneHotEncoder.

    Returns:
        agg_names: list of original feature names
        agg_importances: np.array of same length
    """

    # 1) Extract RF and preprocessor from pipeline
    if not hasattr(model, "named_steps"):
        raise ValueError(
            "Expected a Pipeline with steps 'preprocess' and 'rf'. "
            "Got: {}".format(type(model))
        )

    rf = model.named_steps["rf"]
    preprocessor = model.named_steps["preprocess"]

    importances = rf.feature_importances_

    # 2) Build list of transformed feature names in the same order RF sees them
    #    Our ColumnTransformer has two transformers: ("num", ..., numeric_features) and ("cat", OneHotEncoder, categorical_features)
    ct = preprocessor

    # Numeric: 'passthrough' preserves order, one col per original numeric feature
    transformed_feature_names = []
    for f in numeric_features:
        transformed_feature_names.append(f)

    # Categorical: get dummy names from the fitted OneHotEncoder
    ohe = ct.named_transformers_["cat"]
    # This returns names like 'PropertyType_Condominium', 'Region_Seattle', etc.
    ohe_feature_names = ohe.get_feature_names_out(categorical_features)
    transformed_feature_names.extend(ohe_feature_names)

    if len(transformed_feature_names) != len(importances):
        raise ValueError(
            f"Length mismatch: {len(transformed_feature_names)} transformed "
            f"features vs {len(importances)} importances."
        )

    # 3) Aggregate importances back to original features
    agg = {f: 0.0 for f in numeric_features + categorical_features}

    for name, imp in zip(transformed_feature_names, importances):
        if name in numeric_features:
            # Numeric feature: one-to-one
            agg[name] += float(imp)
        else:
            # One-hot feature, e.g. 'PropertyType_Condominium'
            # Split on first underscore to recover original feature name
            orig = name.split("_", 1)[0]
            if orig in agg:
                agg[orig] += float(imp)

    agg_names = list(agg.keys())
    agg_importances = np.array([agg[n] for n in agg_names])

    return agg_names, agg_importances

def plot_feature_importance_aggregated(
    model,
    numeric_features,
    categorical_features,
    out_path: Path,
):
    """
    Similar to varImpPlot(rf_cv$finalModel) in R,
    but aggregated to original feature level.
    """

    feature_names, agg_importances = get_aggregated_importances(
    model,
    numeric_features=numeric_features,
    categorical_features=categorical_features,
)

    # Sort descending
    idx = np.argsort(agg_importances)
    sorted_names = [feature_names[i] for i in idx]
    sorted_importances = agg_importances[idx]

    plt.figure(figsize=(6, 4))
    plt.barh(sorted_names, sorted_importances)
    plt.xticks(rotation=0)
    plt.yticks(range(len(sorted_names)), sorted_names)
    plt.title("Random Forest Feature Importance (by original feature)")
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()

def plot_partial_dependence_sqft(model, X: pd.DataFrame, out_path: Path):
    """
    Partial dependence for SqFt, similar to:
      p1 <- partial(rf_cv, pred.var = "SqFt", train = data)
      autoplot(..., rug = TRUE)
    """
    fig, ax = plt.subplots(figsize=(6, 4))

    # PDP
    PartialDependenceDisplay.from_estimator(
        model,
        X,
        features=["SqFt"],
        ax=ax,
    )

    # Add rug for SqFt values along x-axis
    sqft_vals = X["SqFt"].values
    # Put rug slightly below the plot's minimum PDP value
    ymin, ymax = ax.get_ylim()
    rug_y = ymin - 0.02 * (ymax - ymin)
    ax.plot(sqft_vals, np.full_like(sqft_vals, rug_y), "|", markersize=4)

    ax.set_title("Partial Dependence: SqFt")
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()

def main():
    print("Loading data...")
    df = load_data()
    print(f"Loaded {len(df)} rows.")

    print("Training model...")
    model, mse = train_with_repeated_cv(df)
    print(f"Validation MSE: {mse:.2f}")

    base_dir = Path(__file__).resolve().parents[1]
    models_dir = base_dir / "models"
    models_dir.mkdir(exist_ok=True)

    model_path = models_dir / "cleaning_fee_rf.pkl"
    joblib.dump(model, model_path)
    print(f"Saved model to {model_path}")


if __name__ == "__main__":
    main()
