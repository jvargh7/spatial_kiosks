library(arrow)
library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(sf)

# Theme for US map
theme_map <- theme_bw(base_size = 14) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14))

#' Plots a US map of the unadjusted blood pressure in each county
plot_pursuant_county_hbp_unadjusted <- function(dt, YEAR_RANGE = "2021-2022"){
  hbp <- dt |>
    filter(year_range == YEAR_RANGE) |>
    group_by(FIPS, county, state) |>
    summarise(hbp = mean(hbp)) |>
    mutate(hbp = 100*hbp) |>
    mutate(hbp_groups = case_when(hbp <40 ~ 1,
                                  hbp < 60 ~ 2,
                                  hbp < 75 ~ 3)) |>
    collect() |>
    mutate(hbp_groups = factor(hbp_groups, levels = 1:3,
                               labels = c("20 to <40", "40 to <60", "60 to <75"))) |>
    mutate(FIPS = sprintf("%05d", FIPS)) 
  
  # Read in previously saved `sf` objects for county/state boundaries
  county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) |>
    left_join(hbp |> dplyr::select(FIPS, hbp, hbp_groups), 
              by = c("GEOID" = "FIPS"))
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  
  # Figure for HBP
  fig_pursuant_hbp <- ggplot() +
    geom_sf(data=county_boundaries,aes(fill = hbp_groups),col=NA)  +
    geom_sf(data=state_boundaries,col="black",fill=NA)  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_manual(name = "Prevalence (%)", 
                      na.value = "grey90", 
                      values = c("20 to <40" = "#449050",
                                 "40 to <60" ="#56B4E9", 
                                 "60 to <75" = "#E69F00") ) +
    theme_map
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
      scale_fill_brewer(palette = "Blues", na.value = "#027324", name = "") +
      theme_map + 
      facet_wrap(~year_range)
  } else{
    nobs <- dt |>
      count(FIPS, county, state, urban) |>
      mutate(n = n ) |>
      mutate(data_value_groups = case_when(n < 250 ~ 1,
                                           n < 500 ~ 2,
                                           n < 1000 ~ 3,
                                           n < 10000 ~ 4,
                                           n < 30000 ~ 5)) |>
      mutate(data_value_groups = factor(data_value_groups, levels = c(1:5),
                                        labels = c(">0 to <250", 
                                                   "250 to <500", 
                                                   "500 to <1K",
                                                   "1K to <10K", 
                                                   "10K to <30K"))  )
  }
  
  
  return(fig_pursuant_nobs)
}