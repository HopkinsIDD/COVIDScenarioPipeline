# COVIDScenarioPipeline
Public shared code for doing scenario forecasting and creating reports for various governmental entities.

# Howto

### Run the code

```
git clone --recurse-submodules SPATIAL_SETUP_REPO
```

If this code change, pull the lastest version of it from the `SPATIAL_SETUP_REPO` using:

```
git submodule foreach git pull origin master
```

After pulling an updated commit of this repository as a submodule, remember to commit that update, since submodules always reference a particular commit.
```
git commit -m "Updated submodule version to master"
```

If making changes to this repository, please do it directly instead of through the submodule.

Then create setup in a subfolder of the `data` using the notebook `data/build-model-input.ipynb`. This creates two files

* `mobility.txt` : mobility matrix from us commute, as in [An Economic Geography of the United States: From Commutes to Megaregions by Garrett Dash Nelson and Alasdair Rae](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0166083) processed by [Ed King on Kaggle](https://www.kaggle.com/kinguistics/visualizing-u-s-commutes)
* `geodata.csv` : specification of the spatial nodes, with at least column for the index, the geoid or name, the population.

Then copy the `main_template.py` from this repo into the root of the SPATIAL_SETUP_REPO folder. Changes the first line and you're ready to run it.

if you haven't installed python packages, run

```
pip3 install pandas numpy seaborn matplotlib geopy tqdm  geopandas shapely numba
```

You can now run the code:
```
nohup python3 main.py > out.txt &
```
creates a `figure/` and a `SCENARIO_model_output_TIMESTAMP` folder.
