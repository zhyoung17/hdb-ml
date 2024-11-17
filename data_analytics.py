import pandas as pd
import numpy as np

df = pd.read_csv('data.csv')
# Convert `month` column into two separate columns: Year and Month
df['year'] = pd.to_datetime(df['month']).dt.year
df['month'] = pd.to_datetime(df['month']).dt.month

# Process `remaining_lease` into years and months as numeric columns
df['remaining_years'] = df['remaining_lease'].str.extract(r'(\d+) years').astype(float)
df['remaining_months'] = df['remaining_lease'].str.extract(r'(\d+) months').astype(float)
df['total_remaining_lease_years'] = df['remaining_years'] + df['remaining_months'] / 12

# Drop intermediate columns
df.drop(columns=['remaining_years', 'remaining_months'], inplace=True)

# Calculate the average resale price for each town for each year starting from 2017
df['year'] = df['year'].astype(int)
df = df[df['year'] >= 2017]
df = df.groupby(['year', 'town'])['resale_price'].mean().reset_index()

# Display the top 5 changes between 2017 to 2024
df_pivot = df.pivot(index='town', columns='year', values='resale_price')
df_pivot['price_change'] = df_pivot[2024] - df_pivot[2017]
df_pivot.sort_values('price_change', ascending=False, inplace=True)
print(df_pivot.head())
