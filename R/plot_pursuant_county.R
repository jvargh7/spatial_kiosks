library(arrow)
library(dplyr)
library(ggplot2)
library(sf)

theme_map <- theme_bw(base_size = 14) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14))

#' Function to plot indicators for Pursuant data at the county level
#' 
#' @param dt arrow dataset
#' @param YEAR_RANGE NULL for all obs, "2017-2018", "2019-2020", etc. for all other year ranges
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

#' Function to plot number of Pursuant observations by county over a year range
#' 
#' @inheritParams plot_pursuant_county_hbp_unadjusted
plot_pursuant_county_nobs <- function(dt, YEAR_RANGE = NULL){
  nobs <- dt |>
    count(FIPS, county, state, urban) |>
    collect() |>
    mutate(FIPS = sprintf("%05d", FIPS)) |>
    mutate(n = n / 1000) |>
    mutate(data_value_groups = case_when(n < 5 ~ 1,
                                         n < 10 ~ 2,
                                         n < 50 ~ 3,
                                         n < 100 ~ 4,
                                         n < 500 ~ 5,
                                         n < 750 ~ 6)) |>
    mutate(data_value_groups = factor(data_value_groups, levels = c(1:6),
                                      labels = c(">0 to <5", 
                                                 "5 to <10", 
                                                 "10 to <50",
                                                 "50 to <100", 
                                                 "100 to <500", 
                                                 "500 to <750"))  )
  
  # Read in previously saved `sf` objects for county/state boundaries
  county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) |>
                        left_join(nobs |> dplyr::select(FIPS, n, data_value_groups), 
                                  by = c("GEOID" = "FIPS"))
  state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  
  # Figure for HBP
  fig_pursuant_nobs <- ggplot() +
    geom_sf(data=county_boundaries,aes(fill = data_value_groups),col=NA)  +
    geom_sf(data=state_boundaries,col="black",fill=NA)  +
    coord_sf(crs = 5070, datum = NA) +
    scale_fill_brewer(palette = "Blues", na.value = "#027324", name = "") +
    theme_map
  return(fig_pursuant_nobs)
}