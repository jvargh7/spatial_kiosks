library(data.table)
library(dplyr)
library(here)
library(tidycensus)

# Create the survey weights by age-gender-ethnicity for each county. 
# Age: 18-19, 20-44, 45-64, 65+
# gender: M/F
# # ethnicity: nh white, nh black, nh asian, nh other, hispanic (race5)
# write_path <- here("data", "reference")
variables_2020 <- load_variables(2020, "pl", cache = TRUE)
vars       <- load_variables(2022, "acs1", cache = TRUE) |> data.table::data.table()
# fwrite(vars, here(write_path, "ACS2022_variables.csv") )

# Load variables related to Hispanic origin and race%
get_prop_NH <- function(){
  race_data <- get_acs(
    geography = "county", # Change to your desired geography level: state, county, tract, etc.
    variables = c(
      "B03002_003", # White alone, not Hispanic or Latino
      "B03002_004", # Black or African American alone, not Hispanic or Latino
      "B03002_005", # American Indian and Alaska Native alone, not Hispanic or Latino
      "B03002_006", # Asian alone, not Hispanic or Latino
      "B03002_007", # Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino
      "B03002_008", # Some other race alone, not Hispanic or Latino
      "B03002_009", # Two or more races, not Hispanic or Latino
      "B03002_012", # Hispanic or Latino
      "B03002_013", # White alone, Hispanic or Latino
      "B03002_014", # Black or African American alone, Hispanic or Latino
      "B03002_015", # American Indian and Alaska Native alone, Hispanic or Latino
      "B03002_016", # Asian alone, Hispanic or Latino
      "B03002_017", # Native Hawaiian and Other Pacific Islander alone, Hispanic or Latino
      "B03002_018", # Some other race alone, Hispanic or Latino
      "B03002_019"  # Two or more races, Hispanic or Latino
    ),
    year = 2022, # Specify the year
    survey = "acs5" # Use ACS 5-year estimates for more precise data
  )
  
  # Reshape data to a wider format to compute proportions
  race_wide <- race_data |>
    dplyr::select(GEOID, NAME, variable, estimate) |> 
    tidyr::pivot_wider(id_cols = c("GEOID", "NAME"), 
                       names_from = "variable",
                       values_from = "estimate")
  
  # Combine the non-Hispanic and Hispanic counts for the "Other" category
  race_wide <- race_wide |>
    dplyr::mutate(
      other_non_hispanic = B03002_005 + B03002_007 + B03002_008 + B03002_009, 
      other_hispanic = B03002_015 + B03002_017 + B03002_018 + B03002_019
    )
  
  # Calculate proportions of non-Hispanic population for each race
  race_wide <- race_wide |>
    dplyr::mutate(
      # white = B03002_003 / (B03002_003 + B03002_013),
      black = B03002_004 / (B03002_004 + B03002_014),
      asian = B03002_006 / (B03002_006 + B03002_016),
      other = other_non_hispanic / (other_non_hispanic + other_hispanic)
    )
  
  # Select and display relevant columns
  race_long <- race_wide |>
    dplyr::select(GEOID, NAME, black, asian, other) |>
    tidyr::pivot_longer(cols = c("black", "asian", "other"), 
                        names_to = "ethnicity", 
                        values_to = "prop_NH")
  
  return(race_long)
}