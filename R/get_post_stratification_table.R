library(data.table)
library(dplyr)
library(here)
library(tidycensus)

# library(survey)
# library(anesrake)

# Create the survey weights by age-gender-ethnicity for each county. 
# Age: 18-19, 20-44, 45-64, 65+
# gender: M/F
# # ethnicity: nh white, nh black, nh asian, nh other, hispanic (race5)
# write_path <- here("data", "reference")
# variables_2020 <- load_variables(2020, "pl", cache = TRUE)
# vars       <- load_variables(2022, "acs1", cache = TRUE) |> data.table::data.table()
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

get_poststrat_table <- function(){
  # Define the age and sex category suffixes for males and females
  male_age_18_19 <- sprintf("%03d", 7)
  female_age_18_19 <- sprintf("%03d", 22)
  
  male_age_20_44 <- sprintf("%03d", 8:11)
  female_age_20_44 <- sprintf("%03d", 23:26)
  
  male_age_45_64 <- sprintf("%03d", 12:13)
  female_age_45_64 <- sprintf("%03d", 27:28)
  
  male_age_65_plus <- sprintf("%03d", 14:16)
  female_age_65_plus <- sprintf("%03d", 29:31)
  
  male_age_categories <- c(male_age_18_19, male_age_20_44, male_age_45_64, male_age_65_plus)
  female_age_categories <- c(female_age_18_19, female_age_20_44, female_age_45_64, female_age_65_plus)
  
  # Define the race/ethnicity suffixes
  race_suffixes <- c("B", "C", "D", "E", "F", "G", "H", "I") # [White alone, Non-Hispanic] (H), Hispanic (I), Black (B), Asian (D)
  
  # Generate variable names for each race and sex by age group
  male_variables <- unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", male_age_categories)))
  female_variables <- unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", female_age_categories)))
  
  variables <- c(male_variables, female_variables)
  
  # Generate the variable names for each group by combining race suffixes with age categories
  # Retrieve the ACS data for the specified variables
  race_data <- get_acs(
    geography = "county", # or "county", "tract", etc.
    variables = variables,
    year = 2022, # Specify the year
    survey = "acs5" # Use ACS 5-year estimates for more precise data
  )
  
  # Create age groupings for each sex and race/ethnicity
  race_data <- race_data |>
    dplyr::mutate(
      age_group = dplyr::case_when(
        # Male and Female age groups 18-19
        variable %in% paste0("B01001", race_suffixes, "_007") |
          variable %in% paste0("B01001", race_suffixes, "_022") ~ "18-19",
        
        # Male and Female age groups 20-44
        variable %in% unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", male_age_20_44))) |
          variable %in% unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", female_age_20_44))) ~ "20-44",
        
        # Male and Female age groups 45-64
        variable %in% unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", male_age_45_64))) |
          variable %in% unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", female_age_45_64))) ~ "45-64",
        
        # Male and Female age groups 65 and over
        variable %in% unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", male_age_65_plus))) |
          variable %in% unlist(lapply(race_suffixes, function(race) paste0("B01001", race, "_", female_age_65_plus))) ~ "65plus",
        
        # # Combine "Other" categories for all age groups
        # variable %in% unlist(lapply(c("C", "E", "F", "G"), function(race) paste0("B01001", race, "_", c(male_age_18_19, female_age_18_19, male_age_20_44, female_age_20_44, male_age_45_64, female_age_45_64, male_age_65_plus, female_age_65_plus)))) ~ "Other",
        
        TRUE ~ NA_character_
      )
    )
  
  # Combine "Other" category data
  other_data <- race_data |>
    dplyr::filter(variable %in% unlist(lapply(c("C", "E", "F", "G"), function(race) paste0("B01001", race, "_", c(male_age_18_19, female_age_18_19, male_age_20_44, female_age_20_44, male_age_45_64, female_age_45_64, male_age_65_plus, female_age_65_plus))))) |> 
    # dplyr::group_by(GEOID, NAME, age_group) |>
    # dplyr::summarize(total_population = sum(estimate, na.rm = TRUE)) |>
    dplyr::mutate(ethnicity = "other") |>
    ungroup()
  
  # Summarize the data for all other races and combine with "Other"
  summary_data <- race_data |>
    filter(!variable %in% unlist(lapply(c("C", "E", "F", "G"), function(race) paste0("B01001", race, "_", c(male_age_18_19, female_age_18_19, male_age_20_44, female_age_20_44, male_age_45_64, female_age_45_64, male_age_65_plus, female_age_65_plus))))) |> 
    mutate(ethnicity = case_when(
      grepl("B01001H", variable) ~ "white",
      grepl("B01001I", variable) ~ "hispanic",
      grepl("B01001B", variable) ~ "black",
      grepl("B01001D", variable) ~ "asian"
    )) |>
    bind_rows(other_data) |>
    mutate(gender = case_when(
      variable %in% male_variables ~ "male",
      variable %in% female_variables ~ "female"
    )) |>
    group_by(GEOID, NAME, age_group, ethnicity, gender) |>
    summarize(n = sum(estimate, na.rm = TRUE)) |>
    ungroup()
  
  # merge in proportion non-Hispanic data
  # apply the NH factor to black alone, asian alone, and other
  # calculate proportions for each county, with full age-sex-race/ethnicity groups
  
  tab_NH <- get_prop_NH()
  
  full_data <- summary_data |>
    left_join(tab_NH[, c("GEOID", "ethnicity", "prop_NH")], by = c("GEOID", "ethnicity")) |>
    mutate(n = ifelse(is.na(prop_NH), n, (n*prop_NH))) |>
    mutate(prop_NH = NULL, NAME = NULL) |>
    rename(FIPS = GEOID)
  return(full_data)
}

# Race --------------------------------------------------------------------
# Retrieve the entire county, state, age group, race/ethnicity, and gender subpopulation weights.
# Joint weights for all variables. Not marginals (like used in raking/IPF)
get_full_poststrat <- function(level = "county", year = 2022){
  
  ages  <- c("18-19", "20", "21", "22-24", "25-29", "30-34", "35-39", "40-44", 
             "45-49", "50-54", "55-59", "60-61", "62-64", 
             "65-66", "67-69", "70-74", "75-79", "80-84", "85plus")
  sexes <- c("male", "female")
  names <- unlist( lapply(sexes, function(s) paste0(s,"_", ages)) )
  
  var_names <- paste0("B01001_",  c(sprintf("%03d", 7:25), sprintf("%03d", 31:49)) )
  names(var_names) <- names
  dt <- get_acs(geography = level, 
                       year = year,
                       variables = var_names,
                       survey = "acs5",
                       # summary_var = "B03002_001", 
                       cache_table = TRUE) |>
    data.table()

  
  pums_data <- get_pums(
    variables = c("AGEP", "SEX", "HISP", "RAC1P"),  # Age, Sex, Hispanic, Race
    state = "GA",  # Replace "GA" with desired state or use "all" for nationwide
    survey = "acs5",
    year = 2022
  )  
}

get_marginal_race_prop <- function(level = "county", year = 2022){
  race <- get_acs(geography = level, 
                       year = year,
                       variables = c(nhwhite = "B03002_003", 
                                     nhblk = "B03002_004", 
                                     nhnative = "B03002_005",
                                     nhasn = "B03002_006", 
                                     nhpacific = "B03002_007",
                                     nhother = "B03002_008",
                                     nhothertwo = "B03002_009",
                                     hisp = "B03002_012"),
                       survey = "acs5",
                       summary_var = "B03002_001", 
                       cache_table = TRUE) |>
    data.table()
  
  race[, variable := ifelse(variable %in% c("nhothertwo", "nhpacific", "nhnative"), "nhother", variable)]
  race <- race[, .(estimate = sum(estimate)), .(GEOID, NAME, variable, summary_est)]
  
  race[, percent := estimate / summary_est]
  race_long <- race
  race_wide <- dcast.data.table(race, GEOID + NAME ~ variable, value.var = "percent")
  
  write_path <- here("data", "reference")
  fwrite(race_wide, here(write_path, paste0("marginal_race_wide_ACS", year, "_", level, ".csv")) )
  fwrite(race_long, here(write_path, paste0("marginal_race_long_ACS", year, "_", level, ".csv")) )
}

# Age/Sex ---------------------------------------------------------------------
get_joint_sex_age_prop <- function(level = "county", year = 2022){
  
  ages  <- c("18-19", "20", "21", "22-24", "25-29", "30-34", "35-39", "40-44", 
             "45-49", "50-54", "55-59", "60-61", "62-64", 
             "65-66", "67-69", "70-74", "75-79", "80-84", "85plus")
  sexes <- c("male", "female")
  names <- unlist( lapply(sexes, function(s) paste0(s,"_", ages)) )
  
  var_names <- paste0("B01001_",  c(sprintf("%03d", 7:25), sprintf("%03d", 31:49)) )
  names(var_names) <- names
  dt <- get_acs(geography = level, 
                       year = year,
                       variables = var_names,
                       survey = "acs5",
                       # summary_var = "B03002_001", 
                       cache_table = TRUE) |>
    data.table()
  
  dt[, c("sex", "age") := tstrsplit(variable, "_", fixed = TRUE)]
  
  age_group_20to44 <- c("20", "21", "22-24", "25-29", "30-34", "35-39", "40-44")
  age_group_45to64 <- c("45-49", "50-54", "55-59", "60-61", "62-64")
  age_group_65plus <- c("65-66", "67-69", "70-74", "75-79", "80-84", "85plus")
  dt[, age := ifelse(age %in% age_group_20to44, "20-44",
                     ifelse(age %in% age_group_45to64, "45-64",
                            ifelse(age %in% age_group_65plus, "65plus", age)
                     ))]
  
  agesex_long <- dt[, .(estimate = sum(estimate)), .(GEOID, NAME, sex, age)]
  agesex_long[, variable := paste0(sex, "_", age)]
  agesex_long[, summary_est := sum(estimate), .(GEOID, NAME)]
  agesex_long[, percent := estimate / summary_est]
  agesex_wide <- dcast.data.table(agesex_long, GEOID + NAME ~ variable, value.var = "percent")
  
  sex_long <- agesex_long[, .(estimate = sum(estimate)), .(GEOID, NAME, sex)]
  sex_long[, summary_est := sum(estimate), .(GEOID, NAME)]
  sex_long[, percent := estimate / summary_est]
  
  sex_wide <- dcast.data.table(sex_long, GEOID + NAME ~ sex, value.var = "percent")
  
  age_long <- agesex_long[, .(estimate = sum(estimate)), .(GEOID, NAME, age)]
  age_long[, summary_est := sum(estimate), .(GEOID, NAME)]
  age_long[, percent := estimate / summary_est]
  
  age_wide <- dcast.data.table(prop_age, GEOID + NAME ~ age, value.var = "percent")
  
  fwrite(agesex_long, here(write_path, paste0("joint_agesex_long_ACS", year, "_", level, ".csv")) )
  fwrite(agesex_wide, here(write_path, paste0("joint_agesex_wide_ACS", year, "_", level, ".csv")) )
  fwrite(age_long, here(write_path, paste0("marginal_age_long_ACS", year, "_", level, ".csv")) )
  fwrite(age_wide, here(write_path, paste0("marginal_age_wide_ACS", year, "_", level, ".csv")) )
  fwrite(sex_long, here(write_path, paste0("marginal_sex_long_ACS", year, "_", level, ".csv")) )
  fwrite(sex_wide, here(write_path, paste0("marginal_sex_wide_ACS", year, "_", level, ".csv")) )
}

# Save to disk ------------------------------------------------------------

# get_marginal_race_prop(level = "county", year = 2022)
# get_marginal_race_prop(level = "state", year = 2022)
# get_marginal_race_prop(level = "us", year  = 2022)
# get_joint_sex_age_prop("county", 2022)
# get_joint_sex_age_prop("state", 2022)
# get_joint_sex_age_prop("us", 2022)