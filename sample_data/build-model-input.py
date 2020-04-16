import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import geopy.distance
from tqdm.auto import tqdm
import itertools, os
sns.set(color_codes=True)
sns.set(rc={'figure.figsize':(5,5)})

#filterUSPS = ['MD']
# filterUSPS = ['MD', 'DC', 'VA', 'DE', 'PA', 'NJ']
filterUSPS = ['CA', 'OR', 'WA']
# foldername = '../../data/around-maryland/'
foldername = 'west-coast/'
#foldername = '../../data/maryland/'

commute_data = pd.read_csv('united-states-commutes/commute_data.csv', dtype={'OFIPS': str, 'DFIPS': str})
census_tracts = pd.read_csv('united-states-commutes/census_tracts_2010.csv', dtype={'GEOID': str})
census_tracts = census_tracts[census_tracts['USPS'].isin(filterUSPS)]
commute_data = commute_data[commute_data['OFIPS'].isin(census_tracts['GEOID'].unique())]
commute_data = commute_data[commute_data['DFIPS'].isin(census_tracts['GEOID'].unique())]
census_tracts = census_tracts.sort_values('POP10', ascending=True)
census_tracts.reset_index(inplace=True)
commute_data.reset_index(inplace=True)

attribution = np.empty(len(census_tracts))
attribution[:] = np.nan
for index, row in tqdm(census_tracts.iterrows(), total=census_tracts.shape[0]):
    attribution[index] = int(str(row['GEOID'])[:5])
census_tracts['county'] = attribution
counties_names = np.unique(attribution)
n_counties = len(counties_names)

pop = np.zeros(n_counties)
state_USPS = []
for i, ct in enumerate(counties_names):
    pop[i] = census_tracts[census_tracts['county'] == ct]['POP10'].sum()
    state_USPS.append(census_tracts[census_tracts['county'] == ct].USPS.iloc[0])

groups = {'geoid': counties_names,
          'pop2010': pop,
          'stateUSPS': state_USPS}
geodata = pd.DataFrame.from_dict(groups)

mobility = np.zeros((n_counties, n_counties))

ori_col = commute_data['OFIPS'].apply(lambda str: float(str[:5]))
dest_col = commute_data['DFIPS'].apply(lambda str: float(str[:5]))

for ori_i, ori_v in tqdm(enumerate(counties_names), total=len(counties_names)):
    for dest_i, dest_v in enumerate(counties_names):
        mobility[ori_i][dest_i] = commute_data[(ori_col == ori_v) & (dest_col == dest_v)]['FLOW'].sum()

np.fill_diagonal(mobility,0)
mobility = mobility + mobility.T # Symetric mobility doubling fluxes mobility.sum is around 5M which is a bit low

if not os.path.exists(foldername):
    os.makedirs(foldername)
np.savetxt(f'{foldername}mobility.txt', mobility)
geodata.to_csv(f'{foldername}geodata.csv', index_label='id')
