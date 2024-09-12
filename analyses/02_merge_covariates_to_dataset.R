library(data.table)
library(dplyr)
library(here)

dt <- fread(here("data", "high_quality_dataset_meanBP.csv"), 
            colClasses = list(character = "FIPS"))

cov <- fread(here("data", "model_covariates.csv"), 
             colClasses = list(character = "FIPS"))

dt_cov <- dt |>
  merge(cov[, .(FIPS, state_region, UE_rate, no_HS_rate, median_income)], by = "FIPS", all.x = TRUE)

fwrite(dt_cov, here("data", "high_quality_dataset_meanBP_w_covariates.csv"))
