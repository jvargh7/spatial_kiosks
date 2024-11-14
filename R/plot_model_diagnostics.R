library(data.table)
library(dplyr)
library(ggplot2)

#' Plots state estimates in order of magnitude to compare to national.  
plot_unadjust_vs_adjust_state <- function(YR = "2021-2022", outcome = "prevalence", stage = "stage2"){
  model_name <- paste(YR, outcome, stage, sep = "_")
  model_est <- here("results", "estimates", paste0(model_name, "_state.csv")  ) |>
    fread() |>
    mutate(type = "Model-adjusted", 
           level = "State")
  
  national_est <- here("results", "estimates", paste0(model_name, "_national.csv")  ) |>
    fread() |>
    mutate(type = "Model-adjusted", 
           level = "National", state = "National")
  
  var <- switch(outcome,
                "prevalence" = paste0("hbp_", stage),
                "awareness" = paste0("hbp_diagnosis"),
                "uncontrolled" = paste0("hbp_diagnosis & hbp_", stage),
                NA) 
  data_est <- here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv") |> 
                  fread(colClasses = list(character = "FIPS")) |>
                  filter(year_range == YR) |>
                  group_by(state) |>
                  summarise(mean = mean(!!rlang::parse_expr(var)), n = n()) |>
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
    ylab(paste0(stringr::str_to_title(paste0(stage, " ", outcome), " (%)"))) +
    geom_hline(data = national, aes(yintercept = mean, col = type), linetype = "dashed") +
    geom_ribbon(data = plot_data, aes(x = state, 
                                      y = (lower + upper) / 2, 
                                      ymin = lower, 
                                      ymax = upper, 
                                      fill = type,
                                      group = type), alpha = 0.2) +
    theme(text = element_text(size = 10)) + 
    ggtitle(model_name)
  
  return(plt)
} 

#' Plots scatterplot of Pursuant county estimates for stage 1 awareness vs. BRFSS 2021
plot_brfss2021_scatter <- function(){
  # Read in CDC Places data
  cdc <- fread(here("data", "raw", "CDC_PLACES.csv"), colClasses = list(character = "LocationID")) |>
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
  est <- here("results", "estimates", "2021-2022_awareness_stage1_county.csv") |> fread(colClasses = list(character = "FIPS")) |>
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
