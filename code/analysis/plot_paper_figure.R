# library(data.table)
# library(dplyr)
library(ggplot2)
library(here)
# library(RColorBrewer)
# library(sf)

source( here("R/plot_pursuant_county.R") )
source( here("R/plot_model_diagnostics.R") )
source( here("R/GLOBAL_VARIABLES.R") )

# Exploratory -------------------------------------------------------------

# Number of observations
plt <- plot_pursuant_county_nobs()
ggsave(here("results/figures/exploratory/map_county_num_obs.png"), width = 11, height = 8.5)

#' Unadjusted hypertension prevalence
plt.stage1.unadjust <- plot_pursuant_county_hbp_unadjusted("stage1")
ggsave(here("results/figures/exploratory/map_county_stage1_unadjust.png"), width = 11, height = 8.5)
plt.stage2.unadjust <- plot_pursuant_county_hbp_unadjusted("stage2")
ggsave(here("results/figures/exploratory/map_county_stage2_unadjust.png"), width = 11, height = 8.5)

# Paper results -----------------------------------------------------------
plt.state.est <- plot_pursuant_state_est(breaks = "regular")
ggsave(here("results/figures/exploratory/map_state_stage2_prevalence_est.png"), width = 11, height = 8.5)

plt.county.est <- plot_pursuant_county_est(breaks = "regular")
ggsave(here("results/figures/exploratory/map_county_stage2_prevalence_est.png"), width = 11, height = 8.5)

# Model diagnostics -------------------------------------------------------
plt.brfss.scatter <- plot_brfss2021_scatter()
ggsave(here("results/figures/diagnostics/scatter_pursuant_vs_brfss2021_stage1_awareness.png"), width = 8, height = 6)

pdf(file = here("results/figures/diagnostics/Unadjusted vs. Adjusted.pdf"), width = 11, height = 6)
for(yr in YEAR_RANGES){
  for(outcome in OUTCOMES){
    for(stage in STAGES){
      plt <- plot_unadjust_vs_adjust_state(YR = yr, outcome = outcome, stage = stage)
      print(plt)
      ggsave( paste0("results/figures/diagnostics/unadjust_vs_adjust/Unadjusted vs. Adjusted ", yr, " ", stringr::str_to_title(paste0(outcome, " ", stage)), ".png"), width = 11, height = 6)
    }
  }
}
dev.off()