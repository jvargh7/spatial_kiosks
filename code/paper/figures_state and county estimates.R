rm(list=ls());gc();source(".Rprofile")
library(here)



source(here("R/figures.R"))


# FIGURES 1 AND 2 ------------------

fig_prevalence_state_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "prevalence", 
                                                       level = "state", breaks = "quantile", 
                                                       range = "one", min = 20, mid = 60, max = 100, var = "mean")

fig_prevalence_county_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "prevalence", 
                                                       level = "county", breaks = "quantile", 
                                                       range = "one", min = 20, mid = 60, max = 100, var = "mean")



fig_awareness_conditional_state_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-conditional", 
                                                       level = "state", breaks = "quantile", 
                                                       range = "one", min = 20, mid = 60, max = 100, var = "mean")

fig_awareness_conditional_county_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-conditional", 
                                                        level = "county", breaks = "quantile", 
                                                        range = "one", min = 20, mid = 60, max = 100, var = "mean")

fig_controlled_state_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "controlled", 
                                                                  level = "state", breaks = "quantile", 
                                                                  range = "one", min = 20, mid = 60, max = 100, var = "mean")

fig_controlled_county_202324 <- plot_pursuant_estimates(stage = "stage2", outcome = "controlled", 
                                                                   level = "county", breaks = "quantile", 
                                                                   range = "one", min = 20, mid = 60, max = 100, var = "mean")


library(ggpubr)

ggarrange(fig_prevalence_state_202324,
          fig_prevalence_county_202324,
          common.legend = TRUE,
          legend = "bottom",
          labels = c("A","B"),
          nrow = 1,
          ncol = 2) %>% 
  ggsave(.,filename=paste0(path_spatial_kiosks_folder,"/figures/state and county prevalence 2023-24.jpg"),width=12,height = 4)

# ggarrange(fig_awareness_conditional_state_202324,
#           fig_awareness_conditional_county_202324,
#           common.legend = TRUE,
#           legend = "bottom",
#           labels = c("A","B"),
#           nrow = 1,
#           ncol = 2) %>% 
#   ggsave(.,filename=paste0(path_spatial_kiosks_folder,"/figures/state and county awareness-conditional 2023-24.jpg"),width=12,height = 4)

ggarrange(fig_awareness_conditional_state_202324,
          fig_awareness_conditional_county_202324,
          fig_controlled_state_202324,
          fig_controlled_county_202324,
          common.legend = TRUE,
          legend = "bottom",
          labels = LETTERS[1:4],
          nrow = 2,
          ncol = 2) %>% 
  ggsave(.,filename=paste0(path_spatial_kiosks_folder,"/figures/state and county awareness-conditional and control 2023-24.jpg"),width=12,height = 8)




# FIGURE 3 ------------------

fig_awareness_marginal_state_202122 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-marginal", 
                                                               level = "state", breaks = "quantile", 
                                                               range = "one", min = 20, mid = 60, max = 100, var = "mean",one_range = "2021-2022")

fig_awareness_marginal_county_202122 <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-marginal", 
                                                                level = "county", breaks = "quantile", 
                                                                range = "one", min = 20, mid = 60, max = 100, var = "mean",one_range = "2021-2022")

fig_awareness_brfss_county <- plot_pursuant_estimates(stage = "stage2", outcome = "awareness-marginal", 
                                                      level = "brfss-county", breaks = "quantile", 
                                                      range = "one", min = 20, mid = 60, max = 100, var = "mean")

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
  ggsave(.,filename=paste0(path_spatial_kiosks_folder,"/figures/awareness-marginal for brfss and pursuant.jpg"),width=12,height = 8)



