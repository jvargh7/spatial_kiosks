library(here)
library(ggplot2)
library(patchwork)

# Variables
STAGES      <- c("stage1", "stage2")
OUTCOMES    <- c("prevalence", "awareness-conditional", "controlled", "awareness-marginal")
YEAR_RANGES <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024")

source(here("R/figures.R"))

# State Unadjust vs. Model-Based Diagnostics ------------------------------
message("Pursuant State Estimates - Direct vs. Model-based")
pdf(file = "results/figures/diagnostics/Pursuant State Estimates - Direct vs. Model-Based.pdf", width = 11, height = 8.5)
for(stage in STAGES){
  for(outcome in OUTCOMES){
     for(year_range in YEAR_RANGES){
       plt <- plot_pursuant_unadjust_vs_adjust_state(stage = stage, outcome = outcome, year_range_arg = year_range)
       print(plt)
     }
  }
}
dev.off()

# Barplots ----------------------------------------------------------------
message("Pursuant Barplots by Gender")
pdf(file = "results/figures/diagnostics/Barplots by Gender.pdf", width = 11, height = 8.5)
for(demo_var in c("age_group", "ethnicity")){
  for(stage in STAGES){
    for(outcome in OUTCOMES){
        plt <- plot_barplot_by_gender(stage = stage, outcome = outcome, demo_var = demo_var, year_range = "All")
        print(plt)
    }
  }
}
dev.off()

# State and County Maps ---------------------------------------------------
message("Pursuant Maps of Estimates")
pdf(file = "results/figures/diagnostics/Pursuant Map Estimates.pdf", height = 8.5, width = 11)
for(stage in STAGES){
  for(outcome in OUTCOMES){
    name <- paste0(YEAR_RANGES, "_", outcome, "_", stage, "_national.csv")
    filepaths <- here("results", "estimates", name)
    dt <- rbindlist(lapply(filepaths, function(filepath) fread(filepath)))
    dt[, mean := ifelse(mean <1, mean*100, mean)]
    
    MID <- median(dt$mean) |> ceiling()
    
    name <- paste0(YEAR_RANGES, "_", outcome, "_", stage, "_county.csv")
    filepaths <- here("results", "estimates", name)
    dt <- rbindlist(lapply(filepaths, function(filepath) fread(filepath)))
    dt[, mean := ifelse(mean <1, mean*100, mean)]
    
    RAD <- abs(dt$mean - MID) |> max() |> ceiling()
    
    plt.state <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MID - RAD, mid = MID, max = MID + RAD, level = "state") 
    plt.county <- plot_pursuant_estimates(stage = stage, outcome = outcome, range = "two", 
                            min = MID - RAD, mid = MID, max = MID + RAD, level = "county")
    
    # divider <- ggplot() +
    #   geom_hline(yintercept = 0, color = "black", size = 1) +
    #   theme_void()  # Remove all axes and background
    
    combined_plot <- (plt.state / plt.county) + 
      plot_layout(guides = "collect", heights = c(1, 1)) +  # Equal height for both plots
      plot_annotation(title = tools::toTitleCase(paste0(stage, " ", outcome)), 
                      theme = theme(legend.position = "bottom",  # Shared legend at the bottom
                                    plot.margin = margin(5, 5, 5, 5)))  # Minimal margins
    
    print(combined_plot)
  }
}
dev.off()

# Reduce whitespace
# Only have min, max text in legend