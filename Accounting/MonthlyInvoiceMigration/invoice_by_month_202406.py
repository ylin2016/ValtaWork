import os
import pandas as pd
import shutil
import glob
import re
from openpyxl import load_workbook

def get_listings(functions_path):
    # Placeholder: You may need to load listings from a file or define them here
    # For now, let's assume cohost$Listing is a list in a CSV or Excel file
    # If you have a file, replace below with actual loading logic
    # Example: pd.read_csv('cohost_listings.csv')['Listing'].unique().tolist()
    return []

def collect_paths(oldloc):
    pathes = []
    loc1 = os.listdir(oldloc)
    for k in loc1:
        loc2 = [d for d in glob.glob(os.path.join(oldloc, k, '*')) if os.path.isdir(d)]
        for d in loc2:
            pathes.append({'loc1': k, 'fullpath': d})
    return pd.DataFrame(pathes)

def collect_files(pathes):
    files = []
    for _, row in pathes.iterrows():
        tmp = os.listdir(row['fullpath'])
        for f in tmp:
            files.append({'loc1': row['loc1'], 'fullpath': row['fullpath'], 'file': f})
    return pd.DataFrame(files)

def extract_property1(files, listings):
    def get_property1(x):
        parts = x.split('_')
        txt2 = parts[1] if len(parts) > 1 else ''
        txt3 = parts[2] if len(parts) > 2 else ''
        txt4 = parts[3] if len(parts) > 3 else ''
        txts = txt4 if txt4 in listings else (txt3 if txt3 in listings else txt2)
        if re.search(r'amazon|Amazon|Costco|Walmart|Home Depot|Home depot', x):
            txts = txt4
        return txts
    files['property1'] = files['file'].apply(get_property1)
    return files

def assign_property(files, listings):
    files['property'] = None
    for idx, row in files.iterrows():
        tmp = False
        for x in listings:
            y = ' '.join(x.split(' ')[::-1])
            if re.search(x, row['file']) or re.search(y, row['file']):
                files.at[idx, 'property'] = x
                break
    files.loc[files['file'].str.contains('Booking.com|booking.com', case=False, regex=True), 'property'] = 'BookingCommission'
    return files

def map_properties(files):
    txts = ["Longbranch","Hoodsport","Keaau","Lilliwaup"] + [f"OSBR {i}" for i in range(1,13)] + ["Ocean Spray 8","Ocean Spray","Microsoft D303","OSBR","Seatac","Osbr","OSBR 11","Burien 14407 Middle","Burien 14407 Top","Beachwood 6","Mercer Island 2449","13020","Microsoft 14620 E205","Microsoft 14645 C19","Microsoft E205","Jing Properties","Seatac 12934","Kirkkland 10219"]
    chngs = ["Longbranch 6821","Hoodsport 26060","Keaau 15-1542","Lilliwaup 28610"] + [f"Cottage {i}" for i in range(1,13)] + ["Cottage 8","OSBR","Microsoft 14615-D303","OSBR","Seatac 12834","OSBR","Cottage 11 (tiny)","Burien 14407 middle","Burien 14407 top","Beachwood 6","Mercer 2449","Bellevue 13020","Microsoft 14620-E205","Microsoft 14645-C19","Microsoft 14620-E205","Seatac 12834","Seatac 12834","Kirkland 10219"]
    for t, c in zip(txts, chngs):
        idx = files['property1'] == t
        files.loc[idx, 'property'] = c
    files.loc[files['file'].str.contains('marketing|Marketing', case=False, regex=True), 'property'] = 'Valta Realty'
    # ...additional mapping logic as in R code...
    return files

def save_tracking_csv(files, drv_loc, month):
    out_path = os.path.join(drv_loc, "Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Filestracking", f"transactions_{month}.csv")
    files.to_csv(out_path, index=False, na_rep='')

def read_folder_paths(drv_loc):
    xlsx_path = os.path.join(drv_loc, "Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Data/FolderPaths.xlsx")
    df = pd.read_excel(xlsx_path)
    df['loc'] = df['loc'].str.replace('2025', '2024')
    return df

def merge_files_loc(files, fileloc):
    files_loc = pd.merge(files, fileloc, left_on='property', right_on='listing', how='left')
    files_loc = files_loc[['property', 'loc', 'file', 'fullpath']]
    return files_loc

def copy_files(files_loc):
    for idx, row in files_loc.iterrows():
        if pd.notna(row['loc']) and row['property'] not in ["Valta Realty", "BookingCommission"]:
            src = os.path.join(row['fullpath'], row['file'])
            dst = os.path.join(row['loc'], "Invoice", row['file'])
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            shutil.copy2(src, dst)
        elif pd.notna(row['loc']) and row['property'] in ["Valta Realty", "BookingCommission"]:
            src = os.path.join(row['fullpath'], row['file'])
            dst = os.path.join(row['loc'], row['file'])
            os.makedirs(row['loc'], exist_ok=True)
            shutil.copy2(src, dst)

def check_new_files(files_loc):
    newfiles = []
    for k in files_loc['loc'].dropna().unique():
        prop = files_loc.loc[files_loc['loc'] == k, 'property']
        if not any(prop.isin(["Valta Realty", "BookingCommission"])):
            tmp = os.listdir(os.path.join(k, "Invoice"))
            newfiles.extend([{'newpath': os.path.join(k, "Invoice"), 'file': f} for f in tmp])
        else:
            tmp = os.listdir(k)
            newfiles.extend([{'newpath': k, 'file': f} for f in tmp])
    newfiles_df = pd.DataFrame(newfiles)
    missing = set(files_loc.loc[files_loc['loc'].notna(), 'file']) - set(newfiles_df['file'])
    return missing

def main():
    Month = "2024-06"
    drv_loc = "/Users/ylin/Google Drive/My Drive/Cohost/"
    oldloc = os.path.join(drv_loc, "Accounting/Company Transactions/2024", Month)
    listings = get_listings(os.path.join(drv_loc, "Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Codes/Functions.R"))
    pathes = collect_paths(oldloc)
    files = collect_files(pathes)
    files = extract_property1(files, listings)
    files = assign_property(files, listings)
    files = map_properties(files)
    save_tracking_csv(files, drv_loc, Month)
    fileloc = read_folder_paths(drv_loc)
    files_loc = merge_files_loc(files, fileloc)
    copy_files(files_loc)
    missing = check_new_files(files_loc)
    print("Missing files:", missing)

if __name__ == "__main__":
    main()
