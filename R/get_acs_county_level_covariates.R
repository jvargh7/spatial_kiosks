library(dplyr)
library(tidycensus)

get_acs_county_level_covariates <- function(){
  
  # Health insurance coverage
  denom <- c(seq(9, 27, 3), seq(37, 55, 3))
  num   <- denom + 1
  
  HI_num_variables   <- paste0("B27001_", sprintf("%03d", num))
  HI_denom_variables <- paste0("B27001_", sprintf("%03d", denom))
  
  acs_num <- tidycensus::get_acs(
    geography = "county",
    variables = HI_num_variables,    # All educational attainment below HS diploma
    # summary_var = "B15003_001",   # Total population 25+
    year = 2022,
    survey = "acs5",
    cache_table = TRUE
  ) |>
     group_by(GEOID) |>
     summarise(num = sum(estimate) )
  
  acs_denom <- tidycensus::get_acs(
    geography = "county",
    variables = HI_denom_variables,    # All educational attainment below HS diploma
    year = 2022,
    survey = "acs5",
    cache_table = TRUE
  ) |>
     group_by(GEOID) |>
     summarise(denom = sum(estimate) )
  
  acs_data_HI <- acs_num |>
    left_join(acs_denom, by = "GEOID") |>
    mutate(HI_coverage = num / denom * 100,
           num = NULL, denom = NULL)
  
  # Median HHI
  acs_data_HHI <- tidycensus::get_acs(
    geography = "county",
    variables = "B19013_001", 
    year = 2022,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    select(GEOID, NAME, estimate) |>
    rename(median_income = estimate)
  
  # Unemployment rate
  acs_data_UE <- tidycensus::get_acs(
    geography = "county",
    variables = "B23025_005",   # Unemployed (Civilian)
    summary_var = "B23025_003", # Labor force (Civilian)
    year = 2022,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    group_by(GEOID) |>
    summarise(UE_rate = sum(estimate) / mean(summary_est) * 100 )
  
  # Educational attainment
  edu_variables <- paste0("B15003_", sprintf("%03d", 2:16))
  acs_data_HS <- tidycensus::get_acs(
    geography = "county",
    variables = edu_variables,    # All educational attainment below HS diploma
    summary_var = "B15003_001",   # Total population 25+
    year = 2022,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    group_by(GEOID) |>
    summarise(no_HS_rate = sum(estimate) / mean(summary_est) * 100 )
  
  acs_data <- acs_data_HHI |>
    left_join(acs_data_HI) |>
    left_join(acs_data_HS) |>
    left_join(acs_data_UE) |>
    mutate(state_code = substr(GEOID, 1, 2)) |>
    rename(FIPS = GEOID) 
  
  return(acs_data)
}

get_state_level_covariates <- function(){
  fips_codes <- tidycensus::fips_codes
  fips       <- unique(fips_codes[, c("state", "state_code", "state_name")]) 
  
  region <- data.frame(state = state.abb, 
                       state_name = state.name,
                       state_region = state.region) |>
    rbind(data.frame(state = "DC", state_name = "District of Columbia", "state_region" = "South"))  |>
    dplyr::left_join(fips[, c("state", "state_code")], by = c("state"))
  
  return(region)
}
