library(data.table)
library(dplyr)
library(here)

dt <- fread(here("data", "high_quality_dataset_meanBP_Sept24.csv"), 
            colClasses = list(character = "FIPS"))

cov <- fread(here("data", "model_covariates.csv"), 
             colClasses = list(character = c("FIPS", "state_code")))

dt_cov <- dt |>
  merge(cov[, .(FIPS, state_region, UE_rate, no_HS_rate, median_income, HI_coverage)], by = "FIPS", all.x = TRUE)

fwrite(dt_cov[!is.na(median_income)], here("data", "high_quality_dataset_meanBP_w_covariates_Sept24.csv"))
