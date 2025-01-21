library(arrow)
library(data.table)
library(here)

# Sourcing table/figure functions -----------------------------------------

source(here("R", "Table1_compute_counts.R"))
source(here("R", "plot_pursuant_county.R"))
source(here("R", "plot_pursuant_hbp_barplot.R"))

if(interactive()){
  dt <- fread( here("data", "high_quality_dataset_meanBP.csv") )
}
# dt <- open_dataset( here("data", "temp", "split"), format = "csv")

bps_table <- dt |>
    filter(age_group %in% c("18-19", "20-44"))|>
    mutate(bps_groups = case_when(bp_systolic < 120 ~ 1,
                                  bp_systolic < 130 ~ 2,
                                  bp_systolic < 140 ~ 3, 
                                  bp_systolic < 150 ~ 4, 
                                  bp_systolic < 160 ~ 5)) |>
  count(bps_groups) |>
  collect() |>
  mutate(percent = n/sum(n))
bpd_table <- dt |>
    mutate(bps_groups = case_when(bp_diastolic < 70 ~ 1,
                                  bp_diastolic < 80 ~ 2,
                                  bp_diastolic < 90 ~ 3, 
                                  bp_diastolic < 100 ~ 4, 
                                  bp_diastolic < 10 ~ 5)) |>
  count(bps_groups) |>
  collect() |>
  mutate(percent = n / sum(n))

# Slide 7: Table1 demographics -----------------------------------------------------
# Will save to ~/tables/Table1.rds
generate_Table1(dt)

# Slide 9: Spatial distribution of Pursuant observations ------------------
fig_pursuant_nobs <- plot_pursuant_county_nobs(dt, YEAR_RANGE = NULL)
fig_pursuant_nobs |>
  ggsave(filename=here("figures", "figure_pursuant_county_nobs_All_HQ.png"), width=6,height=4) 

# Slide 10: Barplot of HBP prevalence by age group/gender --------------------------
fig_pursuant_agegroup_gender_barchart <- plot_pursuant_hbp_by_agegroup_gender_barchart(dt, YEAR_RANGE = "2017-2018") + ggtitle("Pursuant 140/90 (2017-2018)")
fig_pursuant_agegroup_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_agegroup-gender_barchart_2017-2018_HQ.png"),  
         width = 8, height = 4)
fig_pursuant_agegroup_gender_barchart <- plot_pursuant_hbp_by_agegroup_gender_barchart(dt, YEAR_RANGE = "2021-2022") + ggtitle("Pursuant 140/90 (2021-2022)")
fig_pursuant_agegroup_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_agegroup-gender_barchart_2021-2022_HQ.png"),  
         width = 8, height = 4)
fig_pursuant_agegroup_gender_s1_barchart <- plot_pursuant_hbp_by_agegroup_gender_barchart(dt, YEAR_RANGE = "2017-2018", stage1 = TRUE) + ggtitle("Pursuant 130/80 (2017-2018)")
fig_pursuant_agegroup_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp-stage1_by_agegroup-gender_barchart_2021-2022.png"),  
         width = 8, height = 4)

pdf(file = "hbp_outcome_definition_comparison.pdf", width = 8, height = 4)
print(fig_pursuant_agegroup_gender_barchart)
print(fig_pursuant_agegroup_gender_s1_barchart)
dev.off()

# Slide 11: Barplot of HBP prevalence by ethnicity/gender -------------------------
fig_pursuant_ethnicity_gender_barchart <- plot_pursuant_hbp_by_ethnicity_gender_barchart(dt, YEAR_RANGE = "2017-2018")
fig_pursuant_ethnicity_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_ethnicity-gender_barchart_2017-2018_HQ.png"),  
         width = 11, height = 6)
fig_pursuant_ethnicity_gender_barchart <- plot_pursuant_hbp_by_ethnicity_gender_barchart(dt, YEAR_RANGE = "2017-2018", stage1 = TRUE)
fig_pursuant_ethnicity_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp-stage1_by_ethnicity-gender_barchart_2017-2018_HQ.png"),  
         width = 11, height = 6)

# Slide 12: Map of HBP prevalence by county -------------------------------
fig_pursuant_hbp <- plot_pursuant_county_hbp_unadjusted(dt, YEAR_RANGE = "2021-2022")
fig_pursuant_hbp |>
  ggsave(filename=here("figures", "figure_pursuant_county_hbp_unadjusted_2021-2022_HQ.png"), 
         width=6,height=4) 

# Slide Appendix: Barplot of HBP prevalence by urban/rural ----------------
fig_pursuant_agegroup_urban_barchart <- plot_pursuant_hbp_by_agegroup_urban_barchart(dt, YEAR_RANGE = "2017-2018")
fig_pursuant_agegroup_urban_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_agegroup-urban_barchart_2017-2018.png"),  
         width = 8, height = 4)