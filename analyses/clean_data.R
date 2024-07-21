library(arrow)
library(data.table)
library(dplyr)
library(here)

# Main --------------------------------------------------------------------
main <- function(){
  nobs <- vector(mode = "list", length = 7)
  names(nobs) <- c("Start", "Race", "Gender", "Age", "SBP", "DBP", "Deduplicate")
  
  dt   <- open_dataset(here("data", "emory-limited-data-set-export-v2"),format = "parquet") 
  nobs[[1]] <- nrow(dt)
  
  # Mapping from Pursuant addresses to county/FIPS 
  map  <- fread(here("data", "reference", "pursuant_public_kiosk_address_w_county_CT_adj.csv"), 
                colClasses = c(FIPS = 'character'))[, .(street1, street2, city, state, zipcode, 
                                                                                  FIPS, county, urban)] |> unique()
  hbp <- fread(here("data", "supplemental_hbp_cleaned.csv"))
  # Create year  -----------------------------------------
  message("Creating year variable")
  dt <- dt |> 
    mutate(year = substr(session_received_utc, 1, 4)) 
  
  # Ethnicity (Race5: white,black,asian,other,hispanic) ---------------------------------------------------------------
  message("Recoding race/ethnicity")
  dt <- dt |>
    mutate(ethnicity = ifelse(ethnicity == "caucasian", "white", 
                              ifelse(ethnicity == "hispanic_latino", "hispanic", 
                                     ifelse(ethnicity == "american_indian", "native_american",
                                            ifelse(ethnicity == "pacific_islander", "other", 
                                                   # ifelse(ethnicity == "" | ethnicity == 0, "other", 
                                                          ifelse(ethnicity == "native_american", "other", ethnicity))))))
  dt <- dt |>
    mutate(ethnicity = ifelse(ethnicity == "native_american", "other", ethnicity)) |>
    filter(!(ethnicity == "" | ethnicity == 0))
  nobs[[2]] <- nrow(dt)
  
  # Gender ------------------------------------------------------------------
  message("Filter out unspecified gender (blank)")
  dt <- dt |>
    filter(gender != "")
  nobs[[3]] <- nrow(dt)
  
  # Birth year --------------------------------------------------------------
  message("Filter to 18-99 year olds")
  dt <- dt |> 
    mutate(birth_year = as.numeric(birth_year),
           year = as.numeric(year)) |>
    mutate(age = year - birth_year) |>
    filter(between(age, 18, 99))
  nobs[[4]] <- nrow(dt) 
  
  # SBP ---------------------------------------------------------------------
  message("Filter to SBP between 60 and 300")
  dt <- dt |>
    mutate(bp_systolic = as.numeric(bp_systolic)) |>
    filter(between(bp_systolic, 60, 300))
  nobs[[5]] <- nrow(dt)
  # DBP ---------------------------------------------------------------------
  message("Filter to DBP between 30 and 300")
  dt <- dt |>
    mutate(bp_diastolic = as.numeric(bp_diastolic)) |>
    filter(between(bp_systolic, 30, 300))
  nobs[[6]] <- nrow(dt)

  # Merge in supplemental data ----------------------------------------------
  message("Merge in supplemental response data (by session id)")
  dt <- dt |>
    left_join(hbp, by = "session_id_mask")
  
  # Merge geo data (recoding some addresses to match map) ----------------------------------------------------------
  message("Merge in Geo location data")
  dt <- dt |>
    mutate(street1 = gsub("^\\s+|\\s+$", "", street1)) |> # Remove whitespace
    mutate(street1 = ifelse(street1 == "602 Shelia St", "602 SHELIA STREET", street1)) |>
    mutate(street1 = ifelse(street1 == "3101 W Kimberly Road", "3101 W Kimberly Rd", street1)) |>
    mutate(street1 = ifelse(street1 == "9820 Callabridge Court", "9820 Callabridge Ct", street1)) |>
    mutate(street1 = ifelse(street1 == "1600 E Tipton Street", "1600 E Tipton St", street1)) |>
    mutate(street2 = ifelse(street1 %in% c("3209 PINEVILLE MATTHEWS RD", "900 PLEASANT GROVE BLVD"), "", street2)) |>
    mutate(city = ifelse(street1 == "602 SHELIA STREET", "WEST HELENA", city)) |>
    mutate(state = ifelse(state == "Ak", "AK", state)) |>
    left_join(map, by = c("street1", "street2", "city", "state", "zipcode"))
  
  # Age group categories (CDC National Diabetes categories and for raking) ----------------------------------------------------
  # Year ranges (2 yr range) ------------------------------------------------
  dt_save <- dt |>
    mutate(age_group = ifelse(between(age, 20, 44), "20-44", 
                              ifelse(between(age, 45, 64), "45-64",
                                     ifelse(age >=65, "65plus", "18-19")))) |>
    mutate(year_range = ifelse(between(year, 2017, 2018), "2017-2018",
                               ifelse(between(year, 2019, 2020), "2019-2020",
                                      ifelse(between(year, 2021, 2022), "2021-2022", "2023-2024")))) |>
    select(session_id_mask, account_id_mask, pseudo_member_id, 
           #location_name, street1, 
           FIPS, county, state, urban, 
           year, year_range, 
           age, age_group, gender, ethnicity, 
           bp_systolic, bp_diastolic, hbp_diagnosis)

  # Save to disk ------------------------------------------------------------
  message("Collect and save final dataset to disk")
  nobs[[7]] <- nrow(dt_save)
  
  arrow::write_dataset(dt_save, 
                       path = here("data", "kiosk-data-parquet-cleaned-4"), 
                       # partitioning = c("year", "age_group", "gender", "ethnicity", "urban"))
                       partitioning = c("year_range", "gender", "ethnicity", "urban"))
  saveRDS(nobs, here("data", "nobs.rds"))
}

# Takes about 5-7 minutes to run
main()

