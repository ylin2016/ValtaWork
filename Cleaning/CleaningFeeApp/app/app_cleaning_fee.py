from pathlib import Path
import joblib
import pandas as pd
import streamlit as st


# ---------------------------
# Load model
# ---------------------------
@st.cache_resource
def load_model():
    base_dir = Path(__file__).resolve().parents[1]
    model_path = base_dir / "models" / "cleaning_fee_rf.pkl"

    if not model_path.exists():
        raise FileNotFoundError(
            f"Model not found: {model_path}\n"
            "Run: python src/train_cleaning_fee_model.py"
        )

    return joblib.load(model_path)


model = load_model()

# MUST match training script
NUMERIC_FEATURES = [
    "OCCUPANCY",
    "SqFt",
    "BEDROOMS",
    "BEDS",
    "BATHROOMS",
    "avg_rate",
]

CATEGORICAL_FEATURES = [
    "hottub",
    "PropertyType",
    "Region",
]

FEATURE_ORDER = NUMERIC_FEATURES + CATEGORICAL_FEATURES


# Dropdown options — edit these based on your training data
REGION_OPTIONS = [
    "Seattle",
    "Eastside",
    "Burien",
    "HoodCanal",
    "Hawaii",
]

PROPERTY_TYPE_OPTIONS = ['Apartment', 'House', 'Townhouse', 'Condominium', 'Cabin',
       'Bungalow', 'Guesthouse_ADU',
]


# ---------------------------
# Streamlit UI
# ---------------------------
st.set_page_config(
    page_title="Cleaning Fee Estimator",
    page_icon="🧼",
    layout="centered",
)

st.title("Cleaning Fee Estimator 🧼")

col1, col2 = st.columns(2)

with col1:
    occupancy = st.number_input(
        "Max Guests (OCCUPANCY)",
        min_value=1,
        value=4,
        step=1,
    )
    sqft = st.number_input(
        "Square Footage (SqFt)",
        min_value=0.0,
        value=1200.0,
        step=50.0,
    )
    bedrooms = st.number_input(
        "Bedrooms",
        min_value=0,
        value=3,
        step=1,
    )

with col2:
    beds = st.number_input(
        "Beds",
        min_value=0,
        value=4,
        step=1,
    )
    bathrooms = st.number_input(
        "Bathrooms",
        min_value=0.0,
        value=2.0,
        step=0.5,
    )
    avg_rate = st.number_input(
        "Average Daily Rate (avg_rate)",
        min_value=0.0,
        value=200.0,
        step=10.0,
    )

st.markdown("---")

col3, col4, col5 = st.columns(3)

with col3:
    hottub = st.selectbox("Hot Tub", ["No", "Yes"])

with col4:
    property_type = st.selectbox("Property Type", PROPERTY_TYPE_OPTIONS)

with col5:
    region = st.selectbox("Region", REGION_OPTIONS)


# ---------------------------
# Build input DataFrame
# ---------------------------
input_row = {
    "OCCUPANCY": occupancy,
    "SqFt": sqft,
    "BEDROOMS": bedrooms,
    "BEDS": beds,
    "BATHROOMS": bathrooms,
    "avg_rate": avg_rate,
    "hottub": hottub,
    "PropertyType": property_type,
    "Region": region,
}

input_df = pd.DataFrame([[input_row[f] for f in FEATURE_ORDER]], columns=FEATURE_ORDER)

st.subheader("Input Summary")
st.dataframe(input_df)


# ---------------------------
# Prediction
# ---------------------------
if st.button("Estimate Cleaning Fee"):
    try:
        pred = model.predict(input_df)[0]
        st.success(f"Estimated Cleaning Fee: **${pred:,.0f}**")
    except Exception as e:
        st.error(f"Prediction error: {e}")
