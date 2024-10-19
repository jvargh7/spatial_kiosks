library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(RColorBrewer)
library(sf)

# Use same scale for all years. Save it somewhere. 
# Plot County Map (2021-2022)
# Plot State Map (2021-2022) or all years

Figure_state_map <- function(YR = "All", outcome = "prevalence", stage = "stage2"){
  theme_map <- theme_bw(base_size = 14) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14))
  custom_colors <- brewer.pal(5, "YlOrRd")
  
  if(YR != "all"){
    year_ranges <- paste(c("2017-2018", 
                           "2019-2020",
                           "2021-2022",
                           "2023-2024")) 
    dt <- rbindlist(lapply(year_ranges, function(yr){
      name <- paste(yr, outcome, stage, sep = "_") 
      fread(here("results", paste0(name, "_state_fixed-demo.csv")))[, year_range := yr]
    } ))
  } else{
    model_name <- paste(YR, outcome, stage, sep = "_")
    dt <- fread(here("results", paste0(model_name, "_state_fixed-demo.csv")))[, year_range := YR]
  }
  QUANTILES <- quantile(dt$mean, seq(0, 1, .2))
  QUANTILES[1] <- QUANTILES[1] - 0.001
  
  dt[, hbp_groups := cut(mean, breaks = QUANTILES)]
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds")) |>
   left_join(dt, by = c("STUSPS" = "state"))
  
  plt <- ggplot() +
    # geom_sf(data=county_boundaries,aes(fill = hbp_groups))  +
    geom_sf(data=state_boundaries,col="black",aes(fill=hbp_groups))  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_manual(values = custom_colors) + 
    theme_map + 
    facet_wrap(~year_range) + 
    # theme(legend.title = element_text("Prevalence (%)"))
    labs(fill = "Prevalence (%)")
  return(plt)
}

Figure_county_map <- function(YR = "All", outcome = "prevalence", stage = "stage2"){
  theme_map <- theme_bw(base_size = 14) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14))
  custom_colors <- brewer.pal(5, "YlOrRd")
  
  if(YR != "all"){
    year_ranges <- paste(c("2017-2018", 
                           "2019-2020",
                           "2021-2022",
                           "2023-2024")) 
    dt <- rbindlist(lapply(year_ranges, function(yr){
      name <- paste(yr, outcome, stage, sep = "_") 
      fread(here("results", paste0(name, "_county_fixed-demo.csv")), colClasses = list(character = "FIPS"))[, year_range := yr]
    } ))
  } else{
    model_name <- paste(YR, outcome, stage, sep = "_")
    dt <- fread(here("results", paste0(model_name, "_county_fixed-demo.csv")), colClasses = list(character = "FIPS"))[, year_range := YR]
  }
  QUANTILES <- quantile(dt$mean, seq(0, 1, .2))
  QUANTILES[1] <- QUANTILES[1] - 0.001
  
  dt[, hbp_groups := cut(mean, breaks = QUANTILES)]
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) |>
   left_join(dt, by = c("GEOID" = "FIPS"))
  
  plt <- ggplot() +
    geom_sf(data=county_boundaries,aes(fill = hbp_groups), col = NA)  +
    geom_sf(data=state_boundaries,col="black",fill=NA)  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_manual(values = custom_colors) + 
    theme_map + 
    facet_wrap(~year_range) + 
    labs(fill = "Prevalence (%)")
    # theme(legend.title = element_text("Prevalence (%)"))
  return(plt)
}

Figure_state_comparison <- function(YR = "2021-2022", outcome = "prevalence", stage = "stage2"){
  model_name <- paste(YR, outcome, stage, sep = "_")
  model_est <- here("results", paste0(model_name, "_state_fixed-demo.csv")  ) |>
    fread() |>
    mutate(type = "Model-adjusted", 
           level = "State")
  
  national_est <- here("results", paste0(model_name, "_national_fixed-demo.csv")  ) |>
    fread() |>
    mutate(type = "Model-adjusted", 
           level = "National", state = "National")
  
  data_est <- here("data/high_quality_dataset_meanBP_w_covariates_Sept24.csv") |> 
                  fread(colClasses = list(character = "FIPS")) |>
                  filter(year_range == YR) |>
                  group_by(state) |>
                  summarise(mean = mean(hbp_stage2), n = n()) |>
                  ungroup() |>
                  mutate(se = sqrt(mean * (1 - mean) / n)) |>
                  mutate(lower = mean - 1.96*se,
                         upper = mean + 1.96*se) |>
                  mutate(mean = 100*mean,
                         lower = 100*lower,
                         upper = 100*upper,
                         type = "Unadjusted", 
                         level = "State")
  
  national <- data_est |>
    mutate(y = mean * n) |>
    summarise( p = sum(y/100) / sum(n), n = sum(n)) |>
    mutate(se = sqrt(p * (1-p) / n) ) |>
    mutate(lower = p - 1.96*se, upper = p + 1.96*se, type = "Unadjusted", level = "National", 
           state = "National") |>
    mutate(mean = p*100, lower = lower*100, upper = upper*100) |>
    dplyr::select(mean, lower, upper, type) |>
    rbind(national_est[, .(mean, lower, upper, type)])
  
  
  dt <- rbind(model_est, data_est, fill= TRUE)
  state_order <- dt[type == "Model-adjusted"][order(mean), state]
  dt[, state := factor(state, levels = state_order, ordered = TRUE)]
  plot_data <- data.table(state = unique(dt$state), 
                          lower = national$lower[1], 
                          upper = national$upper[1],
                          type = national$type[1])
  plot_data2 <- data.table(state = unique(dt$state), 
                          lower = national$lower[2], 
                          upper = national$upper[2],
                          type = national$type[2])
  plot_data <- rbind(plot_data, plot_data2)
  plt <- ggplot(dt, aes(state, mean, col = type)) + 
    geom_point(alpha = 0.9, position = position_dodge(width = 0.6)) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, 
                  alpha = 0.9, position = position_dodge(width = 0.6)) + 
    theme_bw() + 
    theme(legend.position = "bottom") + 
    xlab(NULL) + 
    ylab("Prevalence (%)") +
    geom_hline(data = national, aes(yintercept = mean, col = type), linetype = "dashed") +
    geom_ribbon(data = plot_data, aes(x = state, 
                                      y = (lower + upper) / 2, 
                                      ymin = lower, 
                                      ymax = upper, 
                                      fill = type,
                                      group = type), alpha = 0.2) +
    theme(text = element_text(size = 10))
  
  return(plt)
} 

Figure_scatter <- function(){
  # Read in CDC Places data
  cdc <- fread(here("data", "CDC_PLACES.csv"), colClasses = list(character = "LocationID")) |>
    dplyr::filter(LocationID != "59") |>
    dplyr::filter(MeasureId == "BPHIGH") |>
    dplyr::filter(Measure == "High blood pressure among adults") |>
    dplyr::select(LocationID, LocationName, StateAbbr, Data_Value_Type, Data_Value, Low_Confidence_Limit, High_Confidence_Limit) |>
    dplyr::rename(FIPS = LocationID, 
           state = StateAbbr,
           cdc_mean = Data_Value,
           lower = Low_Confidence_Limit,
           upper = High_Confidence_Limit) |>
    dplyr::arrange(FIPS, Data_Value_Type) |>
    dplyr::filter(Data_Value_Type == "Age-adjusted prevalence")
  
  # Read in corresponding Pursuant estimates. BRFSS compare to Awareness.
  est <- here("results", "2021-2022_awareness_stage1_county_fixed-demo.csv") |> fread(colClasses = list(character = "FIPS")) |>
    left_join(cdc[, .(FIPS, cdc_mean)]) 
  
  plt <- ggplot(est[state != "FL"], aes(cdc_mean, mean)) + 
    geom_point(alpha=0.5) + 
    geom_smooth(method = "lm") + 
    # geom_smooth()+ 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
    # facet_wrap(~state_region) +
    # facet_wrap(~urban) +
    theme_bw() + 
    xlab("BRFSS 2021 (%)") + 
    ylab("Pursuant 2021-2022 (%)") + 
    theme(text = element_text(size = 16)) + 
    coord_equal()
    
  return(plt)
}

# Figures
figure_state_comp <- Figure_state_comparison(YR  = "2017-2018")
# figure_state_comp <- Figure_state_comparison(YR  = "2019-2020")
# figure_state_comp <- Figure_state_comparison(YR  = "2023-2024")
figure_state_comp <- Figure_state_comparison(YR  = "2021-2022")
ggsave("Figure_state_comp.png", width = 11, height = 6)
figure_state_comp <- Figure_state_comparison(YR  = "2017-2018")
ggsave("Figure_state_comp.png", width = 11, height = 6)
fig_scatter <- Figure_scatter()
ggsave("Figure_scatter.png", width = 8, height = 6)

fig_state_map <- Figure_state_map(stage = "stage1")
ggsave("Figure_state_map_stage1.png", width = 11, height = 8.5)
fig_county_map <- Figure_county_map(stage = "stage1")
ggsave("Figure_county_map_stage1.png", width = 11, height = 8.5)
fig_state_map <- Figure_state_map(stage = "stage2")
ggsave("Figure_state_map_stage2.png", width = 11, height = 8.5)
fig_county_map <- Figure_county_map(stage = "stage2")
ggsave("Figure_county_map_stage2.png", width = 11, height = 8.5)

# Pipeline: 
# Plot of unadjusted estimates vs. MRP (sex, ethnicity, age group)


# Generate barplot estimates by age and sex