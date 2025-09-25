library(here)
library(ggpubr)

source(here("R/figures.R"))

dt.p <- data.table::fread("results/estimates/2023-2024_prevalence_stage2_county.csv")
dt.a <- data.table::fread("results/estimates/2023-2024_awareness-conditional_stage2_county.csv")
dt.a <- data.table::fread("results/estimates/2023-2024_awareness-marginal_stage2_county.csv")
dt.c <- data.table::fread("results/estimates/2023-2024_controlled_stage2_county.csv")

LOW <- 38
HIGH <- 85
MID <- (LOW + HIGH) / 2

BREAKS <- c(40, 50, 60, 70, 80)

# FIGURES 1 AND 2 ------------------

fig_prevalence_state_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "prevalence", 
                                                       level = "state", breaks = BREAKS, 
                                                       range = "one", min = LOW, mid = MID, max = HIGH, var = "mean" )

fig_prevalence_county_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "prevalence", 
                                                       level = "county", breaks = BREAKS, 
                                                       range = "one", min = LOW, mid = MID, max =HIGH, var = "mean")



fig_awareness_conditional_state_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-conditional", 
                                                       level = "state", breaks = BREAKS, 
                                                       range = "one", min = LOW, mid = MID, max = HIGH, var = "mean")

fig_awareness_conditional_county_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-conditional", 
                                                        level = "county", breaks = BREAKS, 
                                                        range = "one", min = LOW, mid = MID, max = HIGH, var = "mean")

fig_controlled_state_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "controlled", 
                                                                  level = "state", breaks = BREAKS, 
                                                                  range = "one", min = LOW, mid = MID, max = HIGH, var = "mean")

fig_controlled_county_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "controlled", 
                                                                   level = "county", breaks = BREAKS, 
                                                                   range = "one", min = LOW, mid = MID, max = HIGH, var = "mean")



# Figure 1 ----------------------------------------------------------------

ggarrange(fig_prevalence_state_202324,
          fig_prevalence_county_202324,
          common.legend = TRUE,
          legend = "bottom",
          labels = c("A","B"),
          nrow = 1,
          ncol = 2) %>% 
  ggsave(.,filename=paste0("results/figures/paper/Figure 1 State and County Prevalence Pursuant 2023-2024.pdf"),width=12,height = 4)
  # ggsave(.,filename=paste0("results/figures/state and county prevalence 2023-24.pdf"),width=12,height = 4)

# Figure 2 ----------------------------------------------------------------

ggarrange(fig_awareness_conditional_state_202324,
          fig_awareness_conditional_county_202324,
          fig_controlled_state_202324,
          fig_controlled_county_202324,
          common.legend = TRUE,
          legend = "bottom",
          labels = LETTERS[1:4],
          nrow = 2,
          ncol = 2) %>% 
  ggsave(.,filename=paste0("results/figures/paper/Figure 2 State and County Awareness and Control Pursuant 2023-2024.pdf"),width=12,height = 8)
  # ggsave(.,filename=paste0("results/figures/state and county awareness-conditional and control 2023-24.pdf"),width=12,height = 8)

# FIGURE 3 ------------------

# fig_awareness_marginal_state_202122 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-marginal", 
#                                                                level = "state", breaks = "quantile", 
#                                                                range = "one", min = 20, mid = 40, max = 60, var = "mean",one_range = "2021-2022")

fig_awareness_marginal_county_202122 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-marginal", 
                                                                level = "county", breaks = c(20, 30, 40, 50, 60), 
                                                                range = "one", min = 20, mid = 40, max = 60, var = "mean",one_range = "2021-2022")

fig_awareness_brfss_county <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-marginal", 
                                                      level = "brfss-county", breaks = c(20, 30, 40, 50, 60), 
                                                      range = "one", min = 20, mid = 40, max = 60, var = "mean")

plt.brfss <- plot_brfss2021_scatter()


ggarrange(ggarrange(fig_awareness_brfss_county,
                    fig_awareness_marginal_county_202122,
                    labels=c("A","B"),
                    legend = "bottom",
                    nrow = 2,ncol=1,
                    common.legend=TRUE),
          plt.brfss,
          labels = c("","C"),
          nrow = 1,
          ncol = 2) %>% 
  ggsave(.,filename=paste0("results/figures/paper/Figure 3 Awareness Marginal for BRFSS and Pursuant.pdf"),width=12,height = 8)



