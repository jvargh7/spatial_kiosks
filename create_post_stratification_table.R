library(data.table)
library(here)
library(tidycensus)

# library(survey)
# library(anesrake)

# Create the survey weights by age-gender-ethnicity for each county. 
# Age: 18-19, 20-44, 45-64, 65+
# gender: M/F
# ethnicity: nh white, nh black, nh asian, nh other, hispanic (race5)
write_path <- here("data", "reference")
vars       <- load_variables(2022, "acs5", cache = TRUE) |> data.table::data.table()
fwrite(vars, here(write_path, "ACS2022_variables.csv") )

# Race --------------------------------------------------------------------
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

get_marginal_race_prop(level = "county", year = 2022)
get_marginal_race_prop(level = "state", year = 2022)
get_marginal_race_prop(level = "us", year  = 2022)
get_joint_sex_age_prop("county", 2022)
get_joint_sex_age_prop("state", 2022)
get_joint_sex_age_prop("us", 2022)