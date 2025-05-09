# Descriptives ----------------
rm(list=ls());gc();source(".Rprofile")

skcl02 <- open_dataset(paste0(path_spatial_kiosks_folder,"/working/data/local/skcl01"),partitioning = "year")  %>% 
  # 02_filter_to_hbp_diagnosis_and_deduplicate.R -----
  dplyr::filter(!is.na(ethnicity)) %>% 
  dplyr::filter(!is.na(gender)) %>% 
  dplyr::filter(between(age,18,99)) %>% 
  dplyr::filter(!is.na(bp_systolic)) %>% 
  dplyr::filter(!is.na(bp_diastolic)) %>% 
  
  
  collect() %>% 
  mutate(age_group = factor(age_group,levels=c(0:4),labels=c("0-17","18-19","20-44","45-64","65 plus"))) %>% 
  mutate(id = case_when(account_id_mask == "" ~ pseudo_member_id,
                        TRUE ~ account_id_mask),
         bmi = as.numeric(bmi),
         weight_lbs = as.numeric(weight_lbs)) %>%
  mutate(bmi = case_when(bmi >=12 & bmi <= 60 ~ bmi,
                         TRUE ~ NA_real_),
         weight_kg = weight_lbs*0.45359237) %>% 
  distinct(year,id,session_id_mask,.keep_all = TRUE) %>% 
  group_by(id, FIPS, #county, 
           state, year, year_range,
           age, age_group, gender, ethnicity, urban) %>% 
  summarize(bp_systolic = mean(bp_systolic,na.rm=TRUE),
            bp_diastolic = mean(bp_diastolic,na.rm=TRUE),
            weight_kg = mean(weight_kg,na.rm = TRUE),
            bmi = mean(bmi,na.rm=TRUE),
            hbp_diagnosis = ifelse(mean(high_blood_pressure_diagnosis_yes) > 0.5, 1, 0)) %>% 
  ungroup() %>% 
  mutate(hbp_bp_stage2 = (bp_systolic >= 140 | bp_diastolic >= 90)*1 ) %>% 
  mutate(hbp_stage2 = case_when( hbp_diagnosis == 1 | hbp_bp_stage2 == 1 ~ 1,
                                 TRUE ~ 0),
         bp_stages = case_when(bp_systolic >= 140 | bp_diastolic >= 90 ~ 4,
                               bp_systolic >= 130 & bp_systolic < 140 ~ 3,
                               bp_diastolic >=80 & bp_diastolic <90 ~ 3,
                               bp_systolic >=120 & bp_diastolic < 80 ~ 2,
                               bp_systolic < 120 & bp_diastolic < 80 ~ 1,
                               TRUE ~ NA_real_),
         ) %>% 
  mutate(bp_stages = factor(bp_stages,levels=c(1:4),c("Normal","Pre","S1","S2"))) %>% 
  dplyr::filter(state != "PR")


skcl02 %>% 
  write_dataset(.,paste0(path_spatial_kiosks_folder,"/working/data/local/skcl02"),partitioning = "year")
