library(data.table)
library(dplyr)
library(here)

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
  diagnosed_htn <- dt[, weighted.mean(toldhighbp == 1, age_weight, na.rm = TRUE)]*100
  stage1_htn    <- dt[, weighted.mean(stage1_hypertension, age_weight, na.rm=TRUE)]*100
  stage2_htn    <- dt[, weighted.mean(stage2_hypertension, age_weight, na.rm=TRUE)]*100
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
       paste0("Diagnosed HTN: ", diagnosed_htn),
       paste0("Stage 1: ", stage1_htn),
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
  
  diagnosed_htn <- pursuant[, weighted.mean(hbp_diagnosis == 1, age_weight, na.rm = TRUE)]*100
  stage1_htn    <- pursuant[, weighted.mean(hbp_stage1, age_weight, na.rm=TRUE)]*100
  stage2_htn    <- pursuant[, weighted.mean(hbp_stage2, age_weight, na.rm=TRUE)]*100
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
       paste0("Diagnosed HTN: ", diagnosed_htn),
       paste0("Stage 1: ", stage1_htn),
       paste0("Stage 2: ", stage2_htn))
}
