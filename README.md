# Creating county food flows

**Objective:** Build `output/dyadic_county_flows_adjusted.rds` and `output/dyadic_county_flows_adjusted.dta` datasets. These datasets are identical (`.rds` is a data object, and `.dta` is a Stata dataset). The following three (3) scripts are required to run in order. Notice that `data/cfs_cleaned_data` and `data/data_needs` are not in repository. Request [Noé J Nava](mailto:noejn2@illinois.edu) these two directories. Other R and Stata scripts are not necessary to build these datasets, but they are complimentary in the study of the reliability of our dataset or to create figures.

### 1) `code/dyadic_county_construction.R`

**Creates:** `output/dyadic_county_2007.rds`

**Requires:** 
- `data/data_needs/location_master_key.csv`
- `data/data_needs/NASS SCTG02 sales by county.dta`
- `data/data_needs/acreage_2017.csv`
- `data_industry_export_cleaned_2017.csv` <sup>[1](#footnote1)</sup>
- `data/county_tools.dta`
- `assets/shp_file/3109_county` these are shapefiles.
- `assets/county_areas.csv`

### 2) `code/state_gravity_reg.do`

**Creates:** `output/dyadic_state_2017_merge.dta`

**Requires:** 
- `data/cfs_cleaned_data`
- `data/data_needs/dyadic_state_2017`
- `data/data_needs/dyadic_state_2012`

### 3) `code/trade_interpolation.R`

**Creates:** `output/dyadic_county_flows_adjusted.rds` and `output/dyadic_county_flows_adjusted.dta`

**Requires:** 
- `output/dyadic_county_2007.rds` <sup>[2](#footnote1)</sup>
- `output/dyadic_state_2017_merge.dta` <sup>[3](#footnote1)</sup>

<sup name="footnote1">1 Please see IMPLAN repository to see how to create this dataset. </sup>.
<sup name="footnote2">2 This is created in step 1 here. </sup>.
<sup name="footnote2">3 This is created in step 2 here. </sup>.