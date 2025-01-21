library(here)
library(ggplot2)
library(tmap)

source(here("R/figures.R"))

# Figure 1: State level stage 2 prevalence --------------------------------
MID <- 52
RAD <- 19

plt.state <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MID - RAD, mid = MID, max = MID + RAD, level = "state", var = "mean")

ggsave(here("results/figures/paper/Figure 1 - State Map of Stage 2 Prevalence.png"), width = 11, height = 8.5)
ggsave(here("results/figures/paper/Figure 1 - State Map of Stage 2 Prevalence.pdf"), width = 11, height = 8.5)

# Figure 2: County level stage 2 prevalence -------------------------------
plt.county <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MID - RAD, mid = MID, max = MID + RAD, level = "county", var = "mean")

ggsave(here("results/figures/paper/Figure 2 - County Map of Stage 2 Prevalence.png"), width = 11, height = 8.5)
ggsave(here("results/figures/paper/Figure 2 - County Map of Stage 2 Prevalence.pdf"), width = 11, height = 8.5)

# Figure 3: BRFSS vs. Pursuant Scatter ------------------------------------
plt.brfss <- plot_brfss2021_scatter()
ggsave(here("results/figures/paper/Figure 3 - BRFSS scatter.png"), width = 8, height = 8)
ggsave(here("results/figures/paper/Figure 3 - BRFSS scatter.pdf"), width = 8, height = 8)

# Figure S1 ---------------------------------------------------------------
plt.PRISM <- plot_PRISM_diagram(here("results/figures/supplement/Figure S1 - PRISM"))

# Figure S2 ---------------------------------------------------------------
plt <- plot_pursuant_nobs()
ggsave(here("results/figures/supplement/Figure S2 - Map of number of observations.png"), width = 11, height = 8.5)
ggsave(here("results/figures/supplement/Figure S2 - Map of number of observations.pdf"), width = 11, height = 8.5)

# Figure S3 ---------------------------------------------------------------
plt.bp <- plot_bp_over_time()
ggsave(here("results/figures/supplement/Figure S3 - Pursuant BP over time.png"), width = 11, height = 6) 
ggsave(here("results/figures/supplement/Figure S3 - Pursuant BP over time.pdf"), width = 11, height = 6)

# Figure S4 - Local Moran's I ---------------------------------------------
plt.moran <- plot_local_moran_I(FALSE)

tmap_save(plt.moran, filename = here("results/figures/supplement/Figure S4 - Local Moran's I.png"), 
            width = 11, height = 5, dpi = 300)
tmap_save(plt.moran, filename = here("results/figures/supplement/Figure S4 - Local Moran's I.pdf"), 
            width = 11, height = 5)