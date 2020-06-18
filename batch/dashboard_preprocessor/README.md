### Data Pre-Processing Script for Dashboard Frontend

This directory contains preprocessing scripts to convert scenario run `parquet` files into `json` files that are readable by the [covid-dashboard-app](https://github.com/HopkinsIDD/covid-dashboard-app) React App. These scripts cut down loading time on the front end by 1 hours per 100 simulations and includes file parsing, data transformations, quantile calculation, and file validation.

Please ensure the tests within this directory pass if any changes to the preprocessing scripts are made 
```
cd batch/dashboard_preprocessor
pytest tests/
```