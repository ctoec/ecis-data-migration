# ecis-data-migration

ETL scripts for migrating data from ECIS and PSR spreadsheets to ECE Reporter.

## Requirements

### SQL Server

```sh
brew tap microsoft/mssql-release https://github.com/Microsoft/homebrew-mssql-release
brew install msodbcsql17 mssql-tools
```

### R

```sh
brew install r
R --slave -e "install.packages(c('tidyverse', 'odbc', 'fuzzyjoin'))"
```

## Connect to source data

### ECIS

1. Copy the ECIS backup files to the `ECIS/` directory.
1. `cp .Renviron.example .Renviron`
1. Update `.Renviron` with the variables defined in `ecis.sh`
1. `./ecis.sh`

### CDC PSRs

Copy PSR spreadsheets to the `PSRs/` directory. All files should be in the XLSX format. File names are not significant.

### State-Funded Programs spreadsheet

Copy the spreadsheet to `All State-Funded Programs Contact List 2019-20.xls`.

## Connect to production database

1. Ensure that your SSH key is available on the production jumpbox.
1. Update `.Renviron` with the database password as `HEDWIG_PROD_DB_PASS`.
1. `./hedwig.sh`

## How to use

The transformation scripts assist with data migration. They should not be considered 100% hands-off, and the transformed data needs to be manually reviewed prior to loading into ECE Reporter.

After completing the above setup steps, open an R console.

```r
source("transform.r")
```

The transformation script will use fuzzy matching to attempt to correct typos and other errors in the source data, particularly in the PSRs. Any imprecise matches will be logged as a warning, as will any other non-blocking errors.

```r
warnings()
```

The data the script plans to load is found in the following dataframes. Review and/or touch up prior to proceeding to the next step.

* `organizations`
* `sites`
* `enrollments`

Note that the `*_id` columns are only for the purposes of the script. When loaded into the production database, different IDs will be assigned to each entity.

For each program that you want to migrate, run the following command using the `organization_id` found in the `organizations` dataframe.

```r
load_organization_to_prod(ORGANIZATION_ID)
```

Note that the script does not check whether the organization already exists. Running `load_organization_to_prod()` multiple times will result in duplicate organizations.

## Todos

* The script cannot identify the Title I status of a site and assumes all sites are not Title I.
* The script currently only handles CDC programs. When School Readiness and other funding sources are added, it is expected that the transformation script will need to be enhanced by, among other things, updating how the script reads from the State-Funded Programs Spreadsheet and incorporating School Readiness PSRs as a data source.
* The script creates a distinct family for each child. When multi-child families are supported by ECE Reporter, the script could attempt to identify children who share a family using last name and income information.
* The Contractor/Subcontractor name as appears on the first sheet of the PSRs could be used as the organization name instead of column G of the State-Funded Programs spreadsheet.
* The script does not attempt to merge the records of students who are missing a SASID on one PSR and subsequently are given a SASID later, as this did not occur in the test data. This could result in duplicate records if not corrected later.
