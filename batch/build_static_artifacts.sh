#!/bin/bash

# This script builds static artifacts by adding post-processed json results to
# covid-dashboard-app source code (cloned github repo) and push to s3 static site

# Clone repository into batch directory
git clone git@github.com:HopkinsIDD/covid-dashboard-app.git

# Copy post-processed json results to covid-dashboard-app's store
cp -R dashboard_preprocessor/results/ covid-dashboard-app/src/store/

# Run build on covid-dashboard-app
cd covid-dashboard-app
# TODO: check if npm is installed? if not npm: brew install npm
npm install
npm run build

# Push built static artifacts to s3 bucket
aws s3 cp build s3://covid-scenario-dashboard/ --recursive

echo "Build static artifacts done"
exit 0