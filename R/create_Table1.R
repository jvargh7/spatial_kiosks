library(arrow)
library(data.table)
library(dplyr)
library(here)
library(srvyr)
library(vroom)

# Age weights from national ACS 2018-2022
source(here("WEIGHTS.R"))

# Completely unweighted
# Age standardize
# Weighted w/ survey weights
# Weighted w/ survey weights + age_standardization
transform_nhanes <- function(dt){
  dt <-  dt |>
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
      ))
  return(dt)
}

#' Generates the Table 1 for NHANES data
generate_nhanes_Table1 <- function(year_range = "2017Mar2020"){
  file     <- paste0("nhanes_", year_range, ".rds")
  filepath <- here("data", "processed", "nhanes", file)
  dt       <- readRDS( filepath ) |>
                transform_nhanes() |>
                left_join(AGE_WEIGHTS, by = "age_group") |>
                data.table()
  
  svyobj <- as_survey_design(.data=dt,
                             ids=psu,
                             strata = pseudostratum,
                             weights = mec2yweight,
                             nest = TRUE,
                             pps = "brewer",variance = "YG")
  
  nhanes_survey <- svyobj |>
      mutate(
        diagnosed = if_else(toldhighbp == 1, 1, 0),
        stage1_awareness= if_else(stage1_hypertension == 1 & diagnosed == 1, 1, 0),
        stage1_control= if_else(stage1_awareness == 1 & SBP < 130 & DBP < 80, 1, 0),
        stage2_awareness = if_else(stage2_hypertension == 1 & diagnosed == 1, 1, 0),
        stage2_control = if_else(stage2_awareness == 1 & SBP < 140 & DBP < 90, 1, 0)
      ) |>
    # group_by(age_group, age_weight) |>
      summarize(
                stage1_prevalence_uw = mean(stage1_hypertension, na.rm=TRUE),
                stage1_diagnosed_uw = mean(stage1_awareness, na.rm=TRUE),
                stage1_awareness_uw = mean(stage1_awareness / stage1_hypertension , na.rm=TRUE),
                stage1_controlled_uw = mean(stage1_control / stage1_awareness, na.rm = TRUE), 
                stage1_prevalence_w = survey_mean(stage1_hypertension, na.rm=TRUE),
                stage1_diagnosed_w = survey_mean(stage1_awareness, na.rm=TRUE),
                stage1_awareness_w = survey_mean(stage1_awareness / stage1_hypertension , na.rm=TRUE),
                stage1_controlled_w = survey_mean(stage1_control / stage1_awareness, na.rm = TRUE),
                stage2_prevalence_uw = mean(stage2_hypertension, na.rm=TRUE),
                stage2_diagnosed_uw = mean(stage2_awareness, na.rm=TRUE),
                stage2_awareness_uw = mean(stage2_awareness / stage2_hypertension , na.rm=TRUE),
                stage2_controlled_uw = mean(stage2_control / stage2_awareness, na.rm = TRUE), 
                stage2_prevalence_w = survey_mean(stage2_hypertension, na.rm=TRUE),
                stage2_diagnosed_w = survey_mean(stage2_awareness, na.rm=TRUE),
                stage2_awareness_w = survey_mean(stage2_awareness / stage2_hypertension , na.rm=TRUE),
                stage2_controlled_w = survey_mean(stage2_control / stage2_awareness, na.rm = TRUE)
      ) 
  
  final_table <- nhanes_survey %>%
    tidyr::pivot_longer(cols = names(.)) %>%
    tidyr::separate(
      name, 
      into = c("stage", "outcome", "weighted", "type"),
      sep = "_"
    ) |>
    tidyr::pivot_wider(id_cols = c("stage", "outcome", "weighted"), 
                       names_from = "type", 
                       values_from = "value") |>
    dplyr::rename(value = `NA`) |>
    mutate(lower = value - 1.96*se, 
           upper = value + 1.96*se) |>
    dplyr::arrange(weighted, stage,outcome)
  
  # diagnosed_htn_uw <- dt[, mean(toldhighbp == 1, na.rm = true)]*100
  # diagnosed_htn    <- dt[, weighted.mean(toldhighbp == 1, age_weight, na.rm = true)]*100
  # stage1_htn_uw    <- dt[, mean(stage1_hypertension, na.rm=true)]*100
  # stage1_htn       <- dt[, weighted.mean(stage1_hypertension, age_weight, na.rm=true)]*100
  # stage2_htn_uw    <- dt[, mean(stage2_hypertension, na.rm=true)]*100
  # stage2_htn       <- dt[, weighted.mean(stage2_hypertension, age_weight, na.rm=true)]*100
  # sbp           <- dt[, weighted.mean(sbp, age_weight, na.rm=true)]
  # dbp           <- dt[, weighted.mean(dbp, age_weight, na.rm=true)]
  
  bp_table <- svyobj |>
    summarize(
              SBP_w_mean = survey_mean(SBP, na.rm=TRUE),
              SBP_w_sd = survey_var(SBP, na.rm=TRUE) |> sqrt(),
              SBP_uw_mean = mean(SBP, na.rm=TRUE),
              SBP_uw_sd = sd(SBP, na.rm=TRUE),
              DBP_w_mean = survey_mean(DBP, na.rm=TRUE),
              DBP_w_sd = survey_var(DBP, na.rm=TRUE) |> sqrt(),
              DBP_uw_mean = mean(DBP, na.rm=TRUE),
              DBP_uw_sd = sd(DBP, na.rm=TRUE),
    ) %>%
    tidyr::pivot_longer(cols = names(.)) %>%
    filter(!grepl("se", name)) |>
    tidyr::separate(
      name, 
      into = c("type", "weighted", "data"),
      sep = "_"
    ) |>
    tidyr::pivot_wider(id_cols = c("type", "weighted"), 
                       names_from = "data", 
                       values_from = "value") |>
    # mutate(lower = mean - 1.96*sd, 
    #        upper = mean + 1.96*sd) |>
    dplyr::arrange(type, weighted)
  
  # sbp           <- dt[, mean(sbp, na.rm=true)]
  # dbp           <- dt[, mean(dbp, na.rm=true)]
  # sbp_sd <- dt[, sd(sbp, na.rm=true)]
  # dbp_sd <- dt[, sd(dbp, na.rm=true)]
  
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
  
  list(
    paste0("Total obs: ", nrow(dt)),
             age_table, 
             gender_table, 
             race_table, 
             # paste0("sbp: ", sbp, " (", sbp_sd, ")"), 
             # paste0("dbp: ", dbp, " (", dbp_sd, ")"),
             # paste0("diagnosed htn (unweighted): ", diagnosed_htn_uw),
             # paste0("diagnosed htn: ", diagnosed_htn),
             # paste0("stage 1 (unweighted): ", stage1_htn_uw),
             # paste0("stage 1: ", stage1_htn),
             # paste0("stage 2 (unweighted): ", stage2_htn_uw),
             # paste0("stage 2: ", stage2_htn))
             final_table,
             bp_table
  )
}

#' generates the table 1 for pursuant data based on year ranges
generate_pursuant_Table1 <- function(year_start=2017, year_end=2020){
  ps       <- fread(here("data/reference/poststratification_table.csv"))[, .(weight = sum(n)), .(age_group, gender, ethnicity)]
  pursuant <- fread(here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv")) |>
    filter(year <= year_end & year >= year_start) |>
    left_join(ps, by = c("age_group", "gender", "ethnicity"))
 
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
  diagnosed_htn    <- pursuant[, weighted.mean(hbp_diagnosis == 1, weight, na.rm = TRUE)]*100
  stage1_htn_uw    <- pursuant[, mean(hbp_stage1, na.rm = TRUE)]*100
  stage1_htn       <- pursuant[, weighted.mean(hbp_stage1, weight, na.rm=TRUE)]*100
  stage2_htn_uw    <- pursuant[, mean(hbp_stage2, na.rm=TRUE)]*100
  stage2_htn       <- pursuant[, weighted.mean(hbp_stage2, weight, na.rm=TRUE)]*100
  # sbp           <- dt[, weighted.mean(sbp, age_weight, na.rm=true)]
  # dbp           <- dt[, weighted.mean(dbp, age_weight, na.rm=true)]
  sbp           <- pursuant[, mean(sbp, na.rm=TRUE)]
  # sbp           <- pursuant[, weighted.mean(sbp, weight, na.rm=TRUE)]
  dbp           <- pursuant[, mean(dbp, na.rm=TRUE)]
  sbp_sd <- pursuant[, sd(sbp, na.rm=TRUE)]
  dbp_sd <- pursuant[, sd(dbp, na.rm=TRUE)]
  
  list(age_table, 
       gender_table, 
       race_table, 
       urban_table,
       paste0("sbp: ", sbp, " (", sbp_sd, ")"), 
       paste0("dbp: ", dbp, " (", dbp_sd, ")"),
       paste0("diagnosed htn (unweighted): ", diagnosed_htn_uw),
       paste0("diagnosed htn: ", diagnosed_htn),
       paste0("stage 1 (unweighted): ", stage1_htn_uw),
       paste0("stage 1: ", stage1_htn),
       paste0("stage 2 (unweighted): ", stage2_htn_uw),
       paste0("stage 2: ", stage2_htn))
}

# generates the table 1 for brfss data for the 2017/2019/2021/2023 surveys
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
      select("_STATE", "_AGEG5YR", "SEX", "_RACE", "_RFHYPE5", "_PSU", "_STSTR", "_LLCPWT") |>
      rename(`_RFHYPE6` = `_RFHYPE5`,
             `_SEX` = SEX)
  } else if(year == 2019){
    brfss_data <- brfss_data |>
      select("_STATE", "_AGEG5YR", "_SEX", "_RACE", "_URBSTAT", "_RFHYPE5", "_PSU", "_STSTR", "_LLCPWT") |>
      rename(`_RFHYPE6` = `_RFHYPE5`) |>
      mutate(urban = case_when(
        `_URBSTAT` == 1 ~ "urban",
        `_URBSTAT` == 2 ~ "rural"
      )) #|>
    # filter(!is.na(urban)) 
  } else if(year == 2021 | year == 2023){
    brfss_data <- brfss_data |>
      select("_STATE", "_AGEG5YR", "_SEX", "_RACE", "_URBSTAT", "_RFHYPE6", "_PSU", "_STSTR", "_LLCPWT") |>
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
    rename(PSU=`_PSU`, 
           STSTR=`_STSTR`,
           LLCPWT=`_LLCPWT`) |>
    filter(!is.na(`_SEX`)) |>
    filter(age_group != "Missing") #|>
    # left_join(AGE_WEIGHTS_SHORT, by = "age_group")
  
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
  options(survey.lonely.psu = "adjust")
  svyobj <- brfss_data %>%
    as_survey_design(.data = .,
                     ids = PSU,
                     strata = STSTR,
                     weights = LLCPWT, 
                     nest = TRUE, 
                     ps = "brewer",variance = "YG")
  
  diagnosed_htn_weighted <- svyobj |>
    summarize(diagnosed = survey_mean(hbp, na.rm=TRUE))
  
  diagnosed_htn_uw       <- brfss_data[, mean(hbp == TRUE)]*100
  # diagnosed_htn_weighted <- brfss_data[, weighted.mean(hbp == TRUE, age_weight)]*100
  
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