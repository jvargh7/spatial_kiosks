rm(list=ls());gc();source(".Rprofile")


source("C:/code/external/functions/nhst/table1_summary.R")


skcl02  <- open_dataset(paste0(path_spatial_kiosks_folder,"/working/data/local/skcl02"),partitioning = "year") %>% 
  collect()


by_year_range_overall = table1_summary(skcl02,
                         g_vars = c("bp_stages"),
                         p_vars = c("hbp_stage2","hbp_diagnosis"),
                         id_vars = c("year_range"))


by_year_range_among_dx = table1_summary(skcl02 %>% dplyr::filter(hbp_diagnosis==1),
                                       g_vars = c("bp_stages"),
                                       id_vars = c("year_range"))

by_year_range_among_nodx = table1_summary(skcl02 %>% dplyr::filter(hbp_diagnosis==0),
                                        g_vars = c("bp_stages"),
                                        id_vars = c("year_range"))



bind_rows(by_year_range_overall %>% mutate(type = "Overall"),
          by_year_range_among_dx %>% mutate(type = "Diagnosed"),
          by_year_range_among_nodx %>% mutate(type = "No Diagnosis")) %>% 
  dplyr::filter(est == "proportion") %>% 
  mutate(group = case_when(is.na(group) ~ variable,
                           TRUE ~ group)) %>% 
  dplyr::select(type,year_range,group,value) %>% 
  pivot_wider(names_from = year_range,values_from=value) %>% 
  write_csv(.,"results/tables/STable2_Unweighted Estimates.csv")





by_year_out <- by_year %>% 
  mutate(selected = case_when(est == "missing" ~ 1,
                              variable %in% c("bp_systolic","bp_diastolic","age","weight_kg","bmi") & est %in% c("mean","sd") ~ 1,
                              # variable %in% c("hba1c","alt","ast") & est %in% c("median","q25","q75") ~ 1,
                              variable == "hbp_stage2" & est %in% c("proportion") ~ 1,
                              variable %in% c("age_group","gender","ethnicity") & est %in% c("freq","proportion") ~ 1,
                              TRUE ~ 0
  )) %>% 
  dplyr::filter(selected == 1) %>% 
  dplyr::select(year,variable,group,est,value,type) %>% 
  mutate(value_cleaned = case_when(est == "proportion" ~ paste0(round(value,1),"%"),
                                   TRUE ~ as.character(round(value,1)))) %>% 
  mutate(variable = case_when(est == "missing" ~ paste0(variable,"_","missing"),
                              TRUE ~ variable)) %>% 
  mutate(est = factor(est,levels=c("mean","sd","median","q25","q75","freq","proportion"))) %>% 
  arrange(year,variable,group,est) %>% 
  group_by(year,variable,group) %>% 
  summarize(output = paste0(value_cleaned,collapse = " [")) %>% 
  ungroup() %>% 
  pivot_wider(names_from=year,values_from=output) %>% 
  dplyr::select(variable,group,everything()) %>% 
  arrange(variable,group) 

write_csv(by_year_out,"data/ekdat01_currently pregnant yes.csv")
