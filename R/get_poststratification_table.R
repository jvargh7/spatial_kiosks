library(data.table)
library(dplyr)
library(here)
library(tidycensus)

# Create the survey weights by age-gender-ethnicity for each county. 
# Age: 18-19, 20-44, 45-64, 65+
# sex: M/F
# ethnicity: nh white, nh black, nh asian, nh other, hispanic (race5)

get_poststratification_table <- function(){
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
