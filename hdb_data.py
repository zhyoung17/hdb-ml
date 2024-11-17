import requests
import pandas as pd

def fetch_and_save_data(resource_id, csv_filename, limit=1000):
    base_url = "https://data.gov.sg/api/action/datastore_search"
    offset = 0
    all_records = []
    total_records_fetched = 0

    while True:
        url = f"{base_url}?resource_id={resource_id}&offset={offset}&limit={limit}"
        response = requests.get(url)
        data = response.json()
        
        records = data['result']['records']
        all_records.extend(records)
        
        records_fetched = len(records)
        total_records_fetched += records_fetched
        print(f"Fetched {records_fetched} records, total so far: {total_records_fetched}")
        
        # Check if we have retrieved all records
        if records_fetched < limit:
            break
        
        offset += limit

    # Convert the accumulated records to a DataFrame
    df = pd.DataFrame(all_records)

    # Save the DataFrame to a CSV file
    df.to_csv(csv_filename, index=False)
    print(f"Data saved to {csv_filename}")

# Usage
resource_id = "d_8b84c4ee58e3cfc0ece0d773c8ca6abc"
csv_filename = "data.csv"
fetch_and_save_data(resource_id, csv_filename)

df = pd.read_csv(csv_filename)
print(df.head())