library(arrow)
library(data.table)
library(dplyr)
library(here)
library(vroom)

# Age weights from national ACS 2018-2022
source(here("WEIGHTS.R"))

#' Generates the Table 1 for NHANES data
generate_nhanes_Table1 <- function(year_range = "2017Mar2020"){
  file <- paste0("nhanes_", year_range, ".rds")
  filepath <- here("data", "processed", "nhanes", file)
  dt <- readRDS( filepath ) |>
    filter(age >= 18) |>
    mutate(ethnicity = case_when(
      race == 1 ~ "Hispanic",
      race == 2 ~ "Hispanic",
      race == 3 ~ "NH White",
      race == 4 ~ "NH Black",
      race == 6 ~ "NH Asian",
      race == 7 ~ "NH Other")) |>
    mutate(age_group = case_when(
      age <=19 ~ "18-19",
      age <45 ~ "20-44",
      age <65 ~ "45-64",
      age >=65 ~ "65plus"
    )) |>
    mutate(gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female"
    )) |>
    mutate(SBP = case_when(
      !is.na(systolic2) & !is.na(systolic3) ~ (systolic2 + systolic3) / 2,
      !is.na(systolic1) & !is.na(systolic2) & is.na(systolic3) ~ (systolic1 + systolic2) / 2,
      is.na(systolic2) & is.na(systolic3) ~ (systolic1)
    )) |>
    mutate(DBP = case_when(
      !is.na(diastolic2) & !is.na(diastolic3) ~ (diastolic2 + diastolic3) / 2,
      !is.na(diastolic1) & !is.na(diastolic2) & is.na(diastolic3) ~ (diastolic1 + diastolic2) / 2,
      is.na(diastolic2) & is.na(diastolic3) ~ (diastolic1)
    )) |>
    mutate(
      stage1_hypertension = case_when(
        # (SBP >= 130 & SBP < 140) | (DBP >= 80 & DBP < 90) ~ TRUE,
        (SBP >= 130 ) | (DBP >= 80 ) ~ TRUE,
        toldhighbp == 1 ~ TRUE,  
        is.na(SBP) & is.na(DBP) ~ NA,  
        TRUE ~ FALSE
    )) |>
    mutate(
      stage2_hypertension = case_when(
        (SBP >= 140 ) | (DBP >= 90 ) ~ TRUE,
        toldhighbp == 1 ~ TRUE,  
        is.na(SBP) & is.na(DBP) ~ NA,  
        TRUE ~ FALSE
    )) |> left_join(AGE_WEIGHTS, by = "age_group") |>
    
    data.table()
  diagnosed_htn_uw <- dt[, mean(toldhighbp == 1, na.rm = TRUE)]*100
  diagnosed_htn    <- dt[, weighted.mean(toldhighbp == 1, age_weight, na.rm = TRUE)]*100
  stage1_htn_uw    <- dt[, mean(stage1_hypertension, na.rm=TRUE)]*100
  stage1_htn       <- dt[, weighted.mean(stage1_hypertension, age_weight, na.rm=TRUE)]*100
  stage2_htn_uw    <- dt[, mean(stage2_hypertension, na.rm=TRUE)]*100
  stage2_htn       <- dt[, weighted.mean(stage2_hypertension, age_weight, na.rm=TRUE)]*100
  # sbp           <- dt[, weighted.mean(SBP, age_weight, na.rm=TRUE)]
  # dbp           <- dt[, weighted.mean(DBP, age_weight, na.rm=TRUE)]
  sbp           <- dt[, mean(SBP, na.rm=TRUE)]
  dbp           <- dt[, mean(DBP, na.rm=TRUE)]
  sbp_sd <- dt[, sd(SBP, na.rm=TRUE)]
  dbp_sd <- dt[, sd(DBP, na.rm=TRUE)]
  
  age_table <- dt |>
    count(age_group) |>
    mutate(percent = n / sum(n)*100)
  
  gender_table <- dt|>
    count(gender) |>
    mutate(percent = n / sum(n)*100)
  
  race_table <- dt |> 
    count(ethnicity) |>
    mutate(percent = n / sum(n)*100)
  race_table <- race_table[c(1,5,3,2,4)]
  
  list(age_table, 
       gender_table, 
       race_table, 
       paste0("SBP: ", sbp, " (", sbp_sd, ")"), 
       paste0("DBP: ", dbp, " (", dbp_sd, ")"),
       paste0("Diagnosed HTN (unweighted): ", diagnosed_htn_uw),
       paste0("Diagnosed HTN: ", diagnosed_htn),
       paste0("Stage 1 (unweighted): ", stage1_htn_uw),
       paste0("Stage 1: ", stage1_htn),
       paste0("Stage 2 (unweighted): ", stage2_htn_uw),
       paste0("Stage 2: ", stage2_htn))
}

#' Generates the Table 1 for Pursuant data based on year ranges
generate_pursuant_Table1 <- function(year_start=2017, year_end=2020){
  pursuant <- fread(here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv")) |>
    filter(year <= year_end & year >= year_start) |>
    left_join(AGE_WEIGHTS, by = "age_group")
 
  age_table <- pursuant |>
    count(age_group) |>
    mutate(percent = n / sum(n) *100)
  
  gender_table <- pursuant |>
    count(gender) |>
    mutate(percent = n / sum(n) *100)
  
  race_table <- pursuant |>
    count(ethnicity) |>
    mutate(percent = n / sum(n) *100)
  
  race_table <- race_table[c(3, 5, 2, 1, 4)]
  
  urban_table <- pursuant |>
    count(urban) |>
    mutate(percent = n / sum(n) *100) |>
    mutate(urban = ifelse(urban == 1, "urban", "rural"))
  
  diagnosed_htn_uw <- pursuant[, mean(hbp_diagnosis == TRUE, na.rm = TRUE)]*100
  diagnosed_htn    <- pursuant[, weighted.mean(hbp_diagnosis == 1, age_weight, na.rm = TRUE)]*100
  stage1_htn_uw    <- pursuant[, mean(hbp_stage1, na.rm = TRUE)]*100
  stage1_htn       <- pursuant[, weighted.mean(hbp_stage1, age_weight, na.rm=TRUE)]*100
  stage2_htn_uw    <- pursuant[, mean(hbp_stage2, na.rm=TRUE)]*100
  stage2_htn       <- pursuant[, weighted.mean(hbp_stage2, age_weight, na.rm=TRUE)]*100
  # sbp           <- dt[, weighted.mean(SBP, age_weight, na.rm=TRUE)]
  # dbp           <- dt[, weighted.mean(DBP, age_weight, na.rm=TRUE)]
  sbp           <- pursuant[, mean(sbp, na.rm=TRUE)]
  dbp           <- pursuant[, mean(dbp, na.rm=TRUE)]
  sbp_sd <- pursuant[, sd(sbp, na.rm=TRUE)]
  dbp_sd <- pursuant[, sd(dbp, na.rm=TRUE)]
  
  list(age_table, 
       gender_table, 
       race_table, 
       urban_table,
       paste0("SBP: ", sbp, " (", sbp_sd, ")"), 
       paste0("DBP: ", dbp, " (", dbp_sd, ")"),
       paste0("Diagnosed HTN (unweighted): ", diagnosed_htn_uw),
       paste0("Diagnosed HTN: ", diagnosed_htn),
       paste0("Stage 1 (unweighted): ", stage1_htn_uw),
       paste0("Stage 1: ", stage1_htn),
       paste0("Stage 2 (unweighted): ", stage2_htn_uw),
       paste0("Stage 2: ", stage2_htn))
}

# Generates the Table 1 for BRFSS data for the 2017/2019/2021/2023 surveys
generate_brfss_Table1 <- function(year = 2021){
  layout_filename <- paste0("data/raw/brfss/variable_layouts/", year, ".csv")
  columns <- fread(here(layout_filename))
  
  columns$File_Width = sapply(1:nrow(columns), function(y) ifelse(y < nrow(columns), 
                                                                  columns$`Starting Column`[y + 1] - columns$`Starting Column`[y], 1))
  
  columns = columns[columns$File_Width > 0,]
  
  # Read the fixed-width file using vroom_fwf
  brfss_filename <- paste0("data/raw/brfss/ascii/LLCP", year, ".ASC ")
  brfss_data <- vroom_fwf(
    file = here(brfss_filename),  # Path to your .ASC file
    col_positions = fwf_widths(columns$File_Width, columns$`Variable Name`)  # Use the column widths and names defined above
    # col_types = vroom::cols(.default = "c")  # Specify column types (optional, here all set to character)
  ) 
  
  if(year == 2017){
    brfss_data <- brfss_data |>
      select("_STATE", "_AGEG5YR", "SEX", "_RACE", "_RFHYPE5") |>
      rename(`_RFHYPE6` = `_RFHYPE5`,
             `_SEX` = SEX)
  } else if(year == 2019){
    brfss_data <- brfss_data |>
      select("_STATE", "_AGEG5YR", "_SEX", "_RACE", "_URBSTAT", "_RFHYPE5") |>
      rename(`_RFHYPE6` = `_RFHYPE5`) |>
      mutate(urban = case_when(
        `_URBSTAT` == 1 ~ "urban",
        `_URBSTAT` == 2 ~ "rural"
      )) #|>
    # filter(!is.na(urban)) 
  } else if(year == 2021 | year == 2023){
    brfss_data <- brfss_data |>
      select("_STATE", "_AGEG5YR", "_SEX", "_RACE", "_URBSTAT", "_RFHYPE6") |>
      mutate(urban = case_when(
        `_URBSTAT` == 1 ~ "urban",
        `_URBSTAT` == 2 ~ "rural"
      )) #|>
    # filter(!is.na(urban)) 
  }
  
  brfss_data <- brfss_data |>
    mutate(sex = case_when( 
      `_SEX` == 1 ~ "Male",
      `_SEX` == 2 ~ "Female")) |>
    mutate(hbp = case_when(
      `_RFHYPE6` == 1 ~ FALSE,
      `_RFHYPE6` == 2 ~ TRUE
    )) |>
    mutate(race = case_when(
      `_RACE` == 1 ~ "NH white",
      `_RACE` == 2 ~ "NH black",
      `_RACE` %in% c(3,5,6,7,9) ~ "NH other",
      `_RACE` == 4 ~ "NH asian",
      `_RACE` == 8 ~ "Hispanic"
    )) |>
    mutate(age_group = case_when(
      `_AGEG5YR` %in% sprintf("%02d", 1:5) ~ "18-44",
      `_AGEG5YR` %in% sprintf("%02d", 6:9) ~ "45-64",
      `_AGEG5YR` %in% sprintf("%02d", 10:13) ~ "65plus",
      `_AGEG5YR` %in% 14 ~ "Missing"
    )) |>
    data.table() |>
    filter(!is.na(hbp)) |>
    filter(!is.na(`_SEX`)) |>
    filter(age_group != "Missing") |>
    left_join(AGE_WEIGHTS_SHORT, by = "age_group")
  
  age_table <- brfss_data |>
    count(age_group) |>
   mutate(percent = n / sum(n) *100)
  
  gender_table <- brfss_data |>
    count(sex) |>
    mutate(percent = n / sum(n) *100)
  
  race_table <- brfss_data |>
    count(race) |>
    mutate(percent = n / sum(n) *100)
  
  race_table <- race_table[c(1,5,3,2,4)]
  
  if(year != 2017){
    urban_table <- brfss_data |>
      count(urban) |>
      mutate(percent = n / sum(n) *100) 
      # mutate(urban = ifelse(urban == 1, "urban", "rural"))
  } else{
    urban_table <- NA
  }

  # Diagnosed hypertension - asked in BRFSS ---------------------------------
  
  diagnosed_htn_uw       <- brfss_data[, mean(hbp == TRUE)]*100
  diagnosed_htn_weighted <- brfss_data[, weighted.mean(hbp == TRUE, age_weight)]*100
  
  list(paste0("Total obs: ", nrow(brfss_data)),
       age_table, 
       gender_table, 
       race_table, 
       urban_table,
       paste0("diagnosed htn unweighted: ", diagnosed_htn_uw),
       paste0("diagnosed htn weighted: ", diagnosed_htn_weighted))
       
  # c("_ageg5yr",  #1-5: 18-44, 6-9: 45-64, 10-13:65 and older, 14: missing
  #   "_sex",     #1=male, 2=female
  #   "_race",     # 1=nh white, 2=nh black, 3,5,6,7,9=nh other, 4=nh asian, 8=hispanic
  #   "_rfhype6", # 1=no, 2=yes, 9=missing
  #   "_urbstat") # urban/rural status 1-urban, 2-rural, 3-dont know
}

# Comparing included and excluded Pursuant observations for Table S1
generate_pursuant_Table1_exclude <- function(year_start = 2017, year_end = 2020){
  pursuant   <- open_dataset(here("data", 
                                  "processed", 
                                  "kiosk-data-parquet-cleaned-Sept24"),
                             format = "parquet")
  
  pursuant_exclude_dt <- pursuant |>
    filter(!(hbp_diagnosis %in% c(0, 1))) |>
    filter(year >= year_start & year <= year_end)
  
  age_group <- pursuant_exclude_dt |>
    count(age_group) |>
    collect() |>
    mutate(percent = n / sum(n) * 100) |>
    arrange(age_group)
  sex <- pursuant_exclude_dt |>
    count(gender) |>
    collect() |>
    mutate(percent = n / sum(n) * 100) |>
    arrange(desc(gender))
  race <- (pursuant_exclude_dt |>
    count(ethnicity) |>
    collect() |>
    mutate(percent = n / sum(n) * 100))[c(3,5,2,1,4),]
    
  urban <- pursuant_exclude_dt |>
    count(urban) |>
    collect() |>
    mutate(percent = n / sum(n) * 100) |>
    mutate(urban = ifelse(urban == 1, "urban", "rural"))
  bp <- pursuant_exclude_dt |>
    summarise(sbp = mean(bp_systolic), sbp_sd = sd(bp_systolic),
              dbp = mean(bp_diastolic), dbp_sd = sd(bp_diastolic)) |>
    collect() 
  nobs <- sum(age_group$n)
  
  list(
    paste0("Total obs: ", nobs),
    age_group, 
       sex, 
       race, 
       urban,
       paste0("SBP: ", bp$sbp, " (", bp$sbp_sd, ")"), 
       paste0("DBP: ", bp$dbp, " (", bp$dbp_sd, ")"))
}