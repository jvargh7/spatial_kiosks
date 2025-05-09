rm(list=ls());gc();source(".Rprofile")

# spatial_kiosks/code/data_acquisition/filter_supplemental_dataset.R -----------
# covariates = readxl::read_excel("data/Health Attributes DLs.xlsx",sheet="Sheet1") %>% 
#   dplyr::select('Data Label') %>% 
#   pull()

dtsup <- open_dataset(paste0(path_spatial_kiosks_folder,"/working/data/raw/emory-limited-data-set-supplemental-export v2"),
                      format = "parquet") |>
  # to_duckdb() %>% 
  dplyr::filter(data_label_name %in%  c("high_blood_pressure_diagnosis_yes","high_blood_pressure_diagnosis_no"), value == 1) %>% 
  mutate(
    date_time = ymd_hms(captured)) %>% 
  collect() 


dtsup_wide <- dtsup %>% 
  arrange(date_time) %>%
  distinct(session_id_mask,data_label_name,value,.keep_all=TRUE) %>% 
  # There are several duplicates (due to different survey_path) so we just take the first one
  pivot_wider(names_from="data_label_name",values_from ="value") %>% 
  mutate(across(.cols=one_of(c("high_blood_pressure_diagnosis_yes")),.fns= ~as.numeric(.))) %>% 
  mutate(high_blood_pressure_diagnosis_yes = case_when(high_blood_pressure_diagnosis_yes == 1 ~ 1,
                                                       high_blood_pressure_diagnosis_no == 1 ~ 0,
                                                       TRUE ~ 0)) 


# spatial_kiosks/code/analysis/01_clean_data.R ------------

dt <- open_dataset(paste0(path_spatial_kiosks_folder,"/working/data/raw/emory-limited-data-set-export v2"),
                   format = "parquet") %>% 
  # Year
  mutate(year = substr(session_received_utc, 1, 4),
         ethnicity = case_when(ethnicity == "" | ethnicity == 0 ~ NA_character_,
                               ethnicity == "caucasian"~"white",
                               ethnicity == "hispanic_latino" ~ "hispanic",
                               ethnicity == "american_indian" ~ "native_american",
                               ethnicity == "pacific_islander" ~  "other",
                               ethnicity == "native_american" ~  "other",
                               TRUE ~ ethnicity),
         gender = case_when(gender == "" ~ NA_character_,
                            TRUE ~ gender)) %>% 
  mutate(birth_year = as.numeric(birth_year),
         year = as.numeric(year)) %>%
  mutate(age = year - birth_year) %>% 
  mutate(bp_systolic = as.numeric(bp_systolic),
         bp_diastolic = as.numeric(bp_diastolic),
         # bmi = as.numeric(bmi)
  ) %>% 
  mutate(bp_systolic = case_when(bp_systolic %in% c(60:300) ~ bp_systolic,
                                 TRUE ~ NA_real_),
         bp_diastolic = case_when(bp_diastolic %in% c(30:300) ~ bp_diastolic,
                                  TRUE ~ NA_real_),
         age_group = case_when(age %in% c(0:17) ~ 0,
                               age %in% c(18,19) ~ 1,
                               age %in% c(20:44) ~ 2,
                               age %in% c(45:64) ~ 3,
                               age %in% c(65:100) ~ 4,
                               TRUE ~ NA_real_),
         year_range = case_when(year %in% c(2017,2018) ~ "2017-2018",
                                year %in% c(2019,2020) ~ "2019-2020",
                                year %in% c(2021,2022) ~ "2021-2022",
                                year %in% c(2023,2024) ~ "2023-2024",
                                TRUE ~ NA_character_)
  )

dt %>% 
  head() %>% 
  collect() %>% 
  View()



# Mapping from Pursuant addresses to county/FIPS 
map  <- read_csv(paste0(path_spatial_kiosks_repo,"/data/reference/pursuant_public_kiosk_address_w_county_CT_adj.csv")) %>% 
  mutate(across(.cols=c("street1", "street2", "city", "state", "zipcode", 
                        "FIPS", "county", "urban"),.fns=~as.character(.))) %>% 
  distinct(FIPS,street1,.keep_all=TRUE)


# Merge in supplemental data ----------------------------------------------
message("Merge in supplemental response data (by session id)")
dt_merged <- dt |>
  dplyr::select(-account_id_mask,-pseudo_member_id) %>% 
  right_join(dtsup_wide %>% 
               dplyr::select(-high_blood_pressure_diagnosis_no), by = "session_id_mask") |>
  mutate(street1 = gsub("^\\s+|\\s+$", "", street1)) |> # Remove whitespace
  mutate(street1 = ifelse(street1 == "602 Shelia St", "602 SHELIA STREET", street1)) |>
  mutate(street1 = ifelse(street1 == "3101 W Kimberly Road", "3101 W Kimberly Rd", street1)) |>
  mutate(street1 = ifelse(street1 == "9820 Callabridge Court", "9820 Callabridge Ct", street1)) |>
  mutate(street1 = ifelse(street1 == "1600 E Tipton Street", "1600 E Tipton St", street1)) |>
  mutate(street2 = ifelse(street1 %in% c("3209 PINEVILLE MATTHEWS RD", "900 PLEASANT GROVE BLVD"), "", street2)) |>
  mutate(city = ifelse(street1 == "602 SHELIA STREET", "WEST HELENA", city)) |>
  mutate(state = ifelse(state == "Ak", "AK", state)) |>
  left_join(map %>% 
              dplyr::select(street1,state,urban,FIPS), by = c("street1","state")) |>
  select(session_id_mask, account_id_mask, pseudo_member_id, 
         session_received_utc, session_started_local_time,
         FIPS, state, county, urban, 
         year, year_range, 
         age, age_group, gender, ethnicity, height_inches, weight_lbs, bmi,
         bp_systolic, bp_diastolic, high_blood_pressure_diagnosis_yes) %>% 
  
  collect() 


dt_merged %>% 
  write_dataset(.,paste0(path_spatial_kiosks_folder,"/working/data/local/skcl01"),partitioning = "year")

dt_merged %>% 
  nrow()
