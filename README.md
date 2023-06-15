# Solana Validator Score 

The `sol-vscore/` folder follows an even/odd structure. Odd numbers are data collection & saves of data (.rds). Even numbers are metrics calculations using the previous odd number collected data. 

A Plumber API is used to programmatically collect data from the original data collection (as early as epoch 332 but consistent full data starts around epoch 444) and save it.

A world map CRS 4326 WGS 84 shapefile is included for mapmaking & identification of country by validator's latitude & longitude. 

- `world-shapefile/`

This API identified Solana Validators at the Voter Public Key `voter_pubkey` level 
across 3 categories of data.

## Application Data

Contains location (coordinates, country), validator name/details, software version, etc. over time measured as Solana epochs (~ 2 days).

- 001_ecosystem_appdata.R
- 001_latest_save.rds
- 002_ecosystem_metrics.R

## Staker Data 

Contains aggregate information on each validator's SOL Staked & number of stakers (`stake_pubkey`) over time including churn statistics. 

- 003_latest_save.rds
- 003_validator_staker_stats.R
- 004_validator_stake_metrics.R

## Vote Data

Aggregation information on each validator's vote behavior including activeness in an epoch and slot confirmation rate (attendance).

- 005_latest_save.rds
- 005_validator_vote_stats.R
- 006_validator_vote_metrics.R


Saved versions are overwritten when an epoch ends and data is updated.

# Reproduce Analysis

All analysis is reproducible using the R programming language. You'll need (1) an shroomDK API key to copy our SQL queries and extract data from the [FlipsideCrypto data app](https://flipsidecrypto.xyz/); and (2) renv to get the exact package versions we used.

## shroomDK

shroomDK is an R package that accesses the FlipsideCrypto RPC API; it is also available for Python. You pass SQL code as a string to our API and get up to 1 GB of data back.

Check out the [documentation](https://docs.flipsidecrypto.com/flipside-api/getting-started) and get your free API Key today.

## renv

renv is a package manager for the R programming language. It ensures analysis is fully reproducible by tracking the exact package versions used in the analysis.

`install.packages('renv')`

## Instructions

To replicate this analysis please do the following:

1.  Clone this repo.
2.  Save your API key into a .txt file as 'api_key.txt' (this exact naming allows the provided .gitignore to ignore your key and keep it off github).
3.  Open the `sol-validator-score.Rproj` R Project file in your R IDE (we recommend, RStudio).
4.  Confirm you have renv installed.
5.  Restore the R environment using `renv::restore()` while in the `sol-validator-score` R Project.
6. Place your api key in a .txt file `api_key.txt` in the `sol-vscore` folder if using the Plumber API, or in the top level if you are running data collection scripts from the top level.
6.  You can now run `001_ecosystem_appdata.R` and other `.R` files. Note, API keys are gated to specific repos, so you may need to use `data_source = "snowflake-default"` once this data is updated for live production data.

If any errors arise, double check you have saved your API key in the expected file name and format.
