library(arrow)
library(here)

# Sourcing table/figure functions -----------------------------------------

source(here("R", "Table1_compute_counts.R"))
source(here("R", "plot_pursuant_county.R"))
source(here("R", "plot_pursuant_hbp_barplot.R"))

dt   <- open_dataset( here("data", "temp", "split"), format = "csv") 

# Slide 7: Table1 demographics -----------------------------------------------------
# Will save to ~/tables/Table1.rds
generate_Table1(dt)

# Slide 9: Spatial distribution of Pursuant observations ------------------
fig_pursuant_nobs <- plot_pursuant_county_nobs(dt, YEAR_RANGE = NULL)
fig_pursuant_nobs |>
  ggsave(filename=here("figures", "figure_pursuant_county_nobs_All.png"), width=6,height=4) 

# Slide 10: Barplot of HBP prevalence by age group/gender --------------------------
fig_pursuant_agegroup_gender_barchart <- plot_pursuant_hbp_by_agegroup_gender_barchart(dt, YEAR_RANGE = "2017-2018")
fig_pursuant_agegroup_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_agegroup-gender_barchart_2017-2018.png"),  
         width = 8, height = 4)

# Slide 11: Barplot of HBP prevalence by ethnicity/gender -------------------------
fig_pursuant_ethnicity_gender_barchart <- plot_pursuant_hbp_by_ethnicity_gender_barchart(dt, YEAR_RANGE = "2017-2018")
fig_pursuant_ethnicity_gender_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_ethnicity-gender_barchart_2017-2018.png"),  
         width = 11, height = 6)

# Slide 12: Map of HBP prevalence by county -------------------------------
fig_pursuant_hbp <- plot_pursuant_county_hbp_unadjusted(dt, YEAR_RANGE = "2021-2022")
fig_pursuant_hbp |>
  ggsave(filename=here("figures", "figure_pursuant_county_hbp_unadjusted_2021-2022.png"), 
         width=6,height=4) 

# Slide Appendix: Barplot of HBP prevalence by urban/rural ----------------
fig_pursuant_agegroup_urban_barchart <- plot_pursuant_hbp_by_agegroup_urban_barchart(dt, YEAR_RANGE = "2017-2018")
fig_pursuant_agegroup_urban_barchart |>
  ggsave(filename = here("figures", "figure_pursuant_hbp_by_agegroup-urban_barchart_2017-2018.png"),  
         width = 8, height = 4)