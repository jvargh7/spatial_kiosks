library(here)
library(dplyr)

source(here("R", "get_acs_county_level_covariates.R"))
source(here("R", "get_post_stratification_table.R"))

# Grab requisite county and state level covariates
dt.county <- get_acs_county_level_covariates()
dt.state  <- get_state_level_covariates()
# Merge in county level Urbanicity covariate from RUCC (not ACS)
rucc      <- readr::read_csv(here("data", "reference", "RUCC2023.csv")) |>
  mutate(urban = ifelse(urban == 1, "urban", "rural"))

# Standardize covariates (county level) and remove PR
dt <- dt.county |>
  dplyr::left_join(dt.state, by = "state_code") |>
  dplyr::filter(state != "PR") |>
  dplyr::mutate(UE_rate = (UE_rate - mean(UE_rate)) / sd(UE_rate),
                HI_coverage = (HI_coverage - mean(HI_coverage)) / sd(HI_coverage),
                no_HS_rate = (no_HS_rate - mean(no_HS_rate)) / sd(no_HS_rate),
                median_income = (median_income - mean(median_income, na.rm = TRUE)) / sd(median_income, na.rm = TRUE) ) |>
  # Loving County, TX
  dplyr::mutate(median_income = ifelse(is.na(median_income), mean(median_income, na.rm = TRUE), median_income)) |>
  left_join(rucc[, c("FIPS", "urban")], by = "FIPS")

data.table::fwrite(dt, here("data", "model_covariates.csv"))

# Retrieve PS table and merge in covariates
ps <- get_poststrat_table()

ps_full <- ps |>
  left_join(dt, by = c("FIPS")) |>
  filter(!is.na(state_name))

data.table::fwrite(ps_full, here("data", "poststratification_table.csv"))
