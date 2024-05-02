rm(list=ls());gc();source(".Rprofile")


emory_limited_dataset_export <- open_dataset(paste0(path_spatial_kiosks_folder,"/working/raw/emory-limited-data-set-export"),format = "parquet") %>% 
  mutate(session_received_utc_dt = ymd_hms(session_started_local_time)) %>% 
  mutate(year = year(session_received_utc_dt))

emory_limited_dataset_supplemental_export <- open_dataset(paste0(path_spatial_kiosks_folder,"/working/raw/emory-limited-data-set-supplemental-export"),format = "parquet")

head_emory_limited_dataset_export <-  emory_limited_dataset_export %>% head(n=1000) %>% collect()
head_emory_limited_dataset_supplemental_export <-  emory_limited_dataset_supplemental_export %>% head(n=1000) %>% collect()

write_csv(head_emory_limited_dataset_export,paste0(path_spatial_kiosks_folder,"/working/raw/head_emory-limited-data-set-export.csv"))
write_csv(head_emory_limited_dataset_supplemental_export,paste0(path_spatial_kiosks_folder,"/working/raw/head_emory-limited-data-set-supplemental-export.csv"))


summary_elde <- emory_limited_dataset_export %>%
  summarize(across(everything(),~sum(!is.na(.)))) %>%
  collect()

summary_eldse <- emory_limited_dataset_supplemental_export %>%
  summarize(across(everything(),~sum(!is.na(.)))) %>%
  collect()  


write_csv(summary_elde,"data/skdat01_counts of available observations in emory limited dataset export.csv")
write_csv(summary_eldse,"data/skdat01_counts of available observations in emory limited dataset supplemental export.csv")


summary_elde_year_state <- emory_limited_dataset_export %>% 
  mutate(session_received_utc_dt = ymd_hms(session_started_local_time)) %>% 
  mutate(year = year(session_received_utc_dt)) %>% 
  group_by(year,state) %>%
  summarize(across(everything(),~sum(!is.na(.)))) %>%
  collect()  
write_csv(summary_elde_year_state,"data/skdat01_counts of available observations by year and state in emory limited dataset export.csv")

summary_eldse_year_state <- emory_limited_dataset_supplemental_export  %>% 
  left_join(emory_limited_dataset_export %>% 
              select(session_id_mask,year,state),
            by = c("session_id_mask")) %>% 
  group_by(year,state,data_label_name) %>%
  summarize(across(everything(),~sum(!is.na(.)))) %>%
  collect()  


write_csv(summary_eldse_year_state,"data/skdat01_counts of available observations by year and state in emory limited dataset supplemental export.csv")


