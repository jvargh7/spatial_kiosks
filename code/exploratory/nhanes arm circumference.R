
rm(list=ls());gc();source(".Rprofile")
path_nhanes_ckm_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NHANES Subtypes Mortality"
path_nhanes_ckm_newdm <- paste0(path_nhanes_ckm_folder,"/working/new diabetes")



library(purrr)
library(readxl)

source("C:/code/external/nhanes_ckm/functions/combine_nhanes.R")


years_to_load <- c("2017Mar2020","20212023")

combined_nhanes <- combine_nhanes(path_nhanes_ckm_folder, years_to_load)


arm_sample = combined_nhanes %>% 
  dplyr::filter(year %in% c("2017Mar2020","20212023")) %>% 
  dplyr::mutate(
    sbp = rowMeans(select(., systolic1, systolic2, systolic3), na.rm = TRUE),  # Calculate mean systolic blood pressure
    dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3), na.rm = TRUE)  # Calculate mean diastolic blood pressure
  ) %>%
  dplyr::select(respondentid,year,psu,pseudostratum,interview_period,age,gender,pregnant, mec2yweight,armcircumference,sbp,dbp,
                toldhighbp2x,toldhighbp,htn_med_told,htn_med_taking,cuffsize, armselected)  %>%
  mutate(pooled_weight = mec2yweight/5.2,
         htn_self_reported = case_when(toldhighbp == 1 ~ 1,
                                       toldhighbp == 2 ~ 0,
                                      TRUE ~ NA_real_),
         female = gender - 1,
         cuffsize_categories = case_when(cuffsize == 2 ~ "9.2 x 16.68 cm",
                                         cuffsize == 3 ~ "12.49 x 23.52 cm",
                                         cuffsize == 4 ~ "14.98 x 31.19 cm",
                                         cuffsize == 5 ~ "17.98 x 37.89 cm",
                                         TRUE ~ NA_character_))

library(srvyr)
arm_svy = arm_sample %>% 
  as_survey_design(ids = psu, strata = pseudostratum, 
                   weights  = pooled_weight, nest =TRUE) %>% 
  srvyr::filter(age >= 18,(pregnant %in% c(2,3) | is.na(pregnant))) %>%
  srvyr::filter(!is.na(htn_self_reported),!is.na(cuffsize_categories))

library(survey)
svyhist(~ armcircumference ,design = arm_svy)

with(arm_sample %>% 
       dplyr::filter(age >= 18,(pregnant %in% c(2,3) | is.na(pregnant))) %>%
       srvyr::filter(!is.na(htn_self_reported),!is.na(cuffsize_categories)),
     hist(armcircumference))

arm_svy %>% 
  group_by(cuffsize_categories) %>% 
  summarize(p = survey_prop(vartype="ci"),
            n = unweighted(n()))
