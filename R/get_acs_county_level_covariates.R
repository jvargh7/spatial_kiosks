get_acs_county_level_covariates <- function(){
  # Set variables
  variables <- c(
    unemployed = "B23025_005",
    labor_force = "B23025_002",
    total_population_25_over = "B15003_001",
    no_high_school = "B15003_002",
    median_income = "B19013_001"
  )
  
  # Retrieve data for all counties
  acs_data <- tidycensus::get_acs(
    geography = "county",
    variables = variables,
    year = 2022,
    survey = "acs5",
    cache_table = TRUE
  )
  
  # Clean and calculate the percentage of residents unemployed and without a high school education
  acs_data_clean <- acs_data |>
    tidyr::pivot_wider(id_cols = c("GEOID", "NAME"), 
                       names_from = "variable",
                       values_from = "estimate") |>
    dplyr::mutate(
      UE_rate = (unemployed / labor_force) * 100,
      no_HS_rate = (no_high_school / total_population_25_over) * 100,
      state_code = substr(GEOID, 1, 2)
    ) |>
    dplyr::select(
      GEOID,
      NAME,
      state_code,
      UE_rate,
      no_HS_rate,
      median_income
    )  |>
    dplyr::rename(FIPS = GEOID)
  return(acs_data_clean)
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
