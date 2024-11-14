library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(RColorBrewer)
library(sf)
library(viridis)

source(here("R/theme_map.R"))

#' Plots a US map of the unadjusted blood pressure in each county for each year range
plot_pursuant_county_hbp_unadjusted <- function(stage = "stage2"){
  dt   <- fread( here("data", "processed", "high_quality_dt_after_deduplication_w_cov_Sept24.csv"),
                 colClasses = list(character = "FIPS"))
  YEAR_RANGES <- unique(dt$year_range) |> sort()
  
  var <- ifelse(stage == "stage1", "hbp_stage1", "hbp_stage2")
  
  hbp <- dt |>
    group_by(FIPS, county, state, year_range) |>
    summarise(hbp = mean(get(var)), n = n()) |>
    filter(n >= 20) |>
    mutate(hbp = 100*hbp) |>
    mutate(hbp_groups = case_when(hbp < 40 ~ 1,
                                  hbp < 50 ~ 2,
                                  hbp < 60 ~ 3,
                                  hbp < 70 ~ 4,
                                  hbp < 80 ~ 5,
                                  hbp < 90 ~ 6,
                                  hbp < 100 ~ 7
                                  )) |>
    mutate(hbp_groups = factor(hbp_groups, levels = 1:7,
                               labels = c("15 to <40", 
                                          "40 to <50", 
                                          "50 to <60",
                                          "60 to <70",
                                          "70 to <80",
                                          "80 to <90",
                                          "90 to <100")))
  
  county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) 
  n_counties <- nrow(county_boundaries)
  county_boundaries <- county_boundaries |>
    slice(rep(1:n(), length(YEAR_RANGES))) |>
    mutate(year_range = rep(YEAR_RANGES, each = n_counties)) |>
    left_join(hbp |> dplyr::select(FIPS, hbp, hbp_groups, year_range), 
              by = c("GEOID" = "FIPS", "year_range")) 
  
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  
  # Figure for HBP
  fig_pursuant_hbp <- ggplot() +
    geom_sf(data=county_boundaries,aes(fill = hbp_groups),col=NA)  +
    geom_sf(data=state_boundaries,col="black",fill=NA)  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_brewer(palette = "YlOrRd", na.value = "#D3D3D3", name = "Prevalence (%)") +
    theme_map + 
    facet_wrap(~year_range) + 
    ggtitle( paste0("Unadjusted ", stage, " hypertension prevalence") )
  
  return(fig_pursuant_hbp)
}

#' Plots a US map of the number of observations in each county. 
plot_pursuant_county_nobs <- function(facet_by_year = TRUE){
  dt   <- fread( here("data", "processed", "high_quality_dt_after_deduplication_w_cov_Sept24.csv"),
                 colClasses = list(character = "FIPS"))
  
  YEAR_RANGES <- unique(dt$year_range) |> sort()
  
  # Facet by the year range
  if(facet_by_year){
    nobs <- dt |>
      count(FIPS, county, state, urban, year_range) |>
      mutate(data_value_groups = case_when(
        n < 30 ~ 1,
        n < 60 ~ 2,
        n < 90 ~ 3,
        n < 180 ~ 4,
        n < 360 ~ 5,
        n <= 13720 ~ 6)) |>
      mutate(data_value_groups = factor(data_value_groups, levels = c(1:6),
                                        labels = c(">0 to <30", 
                                                   "30 to <60",
                                                   "60 to <90", 
                                                   "90 to <180", 
                                                   "180 to <360", 
                                                   "360 to 13717")))
    # Read in previously saved `sf` objects for county/state boundaries
    county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) 
    n_counties <- nrow(county_boundaries)
    county_boundaries <- county_boundaries |>
      slice(rep(1:n(), length(YEAR_RANGES))) |>
      mutate(year_range = rep(YEAR_RANGES, each = n_counties)) |>
      left_join(nobs |> dplyr::select(FIPS, n, data_value_groups, year_range), 
                by = c("GEOID" = "FIPS", "year_range")) 
    
    state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
    
    # Figure for HBP
    fig_pursuant_nobs <- ggplot() +
      geom_sf(data=county_boundaries,aes(fill = data_value_groups),col=NA)  +
      geom_sf(data=state_boundaries,col="black",fill=NA)  +
      coord_sf(crs = 5070, datum = NA) +
      # scale_fill_brewer(palette = "Blues", na.value = "#027324", name = "") +
      # scale_fill_brewer(palette = "Blues", na.value = "#D3D3D3", name = "") +
      scale_fill_viridis(
        discrete = TRUE,           # Use discrete scale for ordered factor
        option = "C",              # "C" is one of the options in viridis for sequential colors
        direction = 1,             # Controls increasing color intensity
        na.value = "#D3D3D3",      # Light gray for missing or no data
        name = "Number of observations"        # Legend title
      ) +
      theme_map + 
      facet_wrap(~year_range)
  } else{
    # nobs <- dt |>
    #   count(FIPS, county, state, urban) |>
    #   mutate(n = n ) |>
    #   mutate(data_value_groups = case_when(n < 250 ~ 1,
    #                                        n < 500 ~ 2,
    #                                        n < 1000 ~ 3,
    #                                        n < 10000 ~ 4,
    #                                        n < 30000 ~ 5)) |>
    #   mutate(data_value_groups = factor(data_value_groups, levels = c(1:5),
    #                                     labels = c(">0 to <250", 
    #                                                "250 to <500", 
    #                                                "500 to <1K",
    #                                                "1K to <10K", 
    #                                                "10K to <30K"))  )
  }
  
  return(fig_pursuant_nobs)
}

#' Plots a US state map of model estimates for outcome (awareness, prevalence) and stage (1 or 2)
plot_pursuant_state_est <- function(outcome = "prevalence", stage = "stage2", breaks = "quantile"){
  year_ranges <- paste(c("2017-2018", 
                           "2019-2020",
                           "2021-2022",
                           "2023-2024")) 
  dt <- rbindlist(lapply(year_ranges, function(yr){
      name <- paste(yr, outcome, stage, sep = "_") 
      fread(here("results", "estimates",
                 paste0(name, "_state.csv")))[, year_range := yr]
  }))
  
  if(breaks == "quantile"){
    custom_colors <- brewer.pal(5, "YlOrRd")
    QUANTILES <- quantile(dt$mean, seq(0, 1, .2))
    QUANTILES[1] <- QUANTILES[1] - 0.001
    BREAKS <- QUANTILES
  } else if(breaks == "regular"){
    # We want these to be the same as those we will use for counties
    custom_colors <- brewer.pal(5, "YlOrRd")
    MIN <- min(dt$mean)-0.001
    MAX <- max(dt$mean)
    BREAKS <- seq(MIN, MAX, length.out = 6)
  }
  
  dt[, hbp_groups := cut(mean, breaks = BREAKS)]
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  n_states <- nrow(state_boundaries)
  state_boundaries <- state_boundaries |>
    slice(rep(1:n(), length(year_ranges))) |>
    mutate(year_range = rep(year_ranges, each = n_states)) |>
    left_join(dt, by = c("STUSPS" = "state", "year_range"))
  
  plt <- ggplot() +
    geom_sf(data=state_boundaries,col="black",aes(fill=hbp_groups))  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_manual(values = custom_colors) + 
    # scale_fill_manual(values = custom_colors) + 
    theme_map + 
    facet_wrap(~year_range) + 
    # theme(legend.title = element_text("Prevalence (%)"))
    labs(fill = paste0(outcome, " (%)")) + 
    ggtitle(tools::toTitleCase(paste0(stage, " ", outcome, " (", breaks, ")")) )
  return(plt)
}

#' Plots a US county map of model estimates for outcome (awareness, prevalence) and stage (1 or 2)
plot_pursuant_county_est <- function(outcome = "prevalence", stage = "stage2", breaks = "quantile"){
  year_ranges <- paste(c("2017-2018", 
                           "2019-2020",
                           "2021-2022",
                           "2023-2024")) 
  dt <- rbindlist(lapply(year_ranges, function(yr){
      name <- paste(yr, outcome, stage, sep = "_") 
      fread(here("results", "estimates",
                 paste0(name, "_county.csv")), colClasses = list(character = "FIPS"))[, year_range := yr]
  }))
  
  if(breaks == "quantile"){
    custom_colors <- brewer.pal(5, "YlOrRd")
    QUANTILES <- quantile(dt$mean, seq(0, 1, .2))
    QUANTILES[1] <- QUANTILES[1] - 0.001
    BREAKS <- QUANTILES
  } else if(breaks == "regular"){
    # We want these to be the same as those we will use for counties
    custom_colors <- brewer.pal(5, "YlOrRd")
    MIN <- min(dt$mean)-0.001
    MAX <- max(dt$mean)
    BREAKS <- seq(MIN, MAX, length.out = 6)
  }
  dt[, hbp_groups := cut(mean, breaks = BREAKS)]
  
  county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) 
  n_counties <- nrow(county_boundaries)
  county_boundaries <- county_boundaries |>
    slice(rep(1:n(), length(year_ranges))) |>
    mutate(year_range = rep(year_ranges, each = n_counties)) |>
    left_join(dt, 
              by = c("GEOID" = "FIPS", "year_range")) 
    
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  
  plt <- ggplot() +
    geom_sf(data=county_boundaries,aes(fill = hbp_groups), col = NA)  +
    geom_sf(data=state_boundaries,col="black",fill=NA)  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_manual(values = custom_colors) + 
    # scale_fill_manual(values = custom_colors) + 
    theme_map + 
    facet_wrap(~year_range) + 
    # theme(legend.title = element_text("Prevalence (%)"))
    labs(fill = paste0(outcome, " (%)")) + 
    ggtitle(tools::toTitleCase(paste0(stage, " ", outcome, " (", breaks, ")")) )
  return(plt)
}
