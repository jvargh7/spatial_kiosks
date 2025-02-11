library(arrow)
library(data.table)
library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)
library(ggplot2)
library(here)
library(patchwork)
library(RColorBrewer)
library(rsvg)
library(sf)
library(spdep)
library(tmap)
library(viridis)

source(here("R/theme_map.R"))

# Functions ---------------------------------------------------------------
plot_PRISM_diagram      <- function(filename){
  diag <- grViz("
  digraph PRISMA {
    
    graph [layout = dot, rankdir = TB]
    
    # Define nodes
    node [shape = rectangle, style = filled, color = lightgrey]
    A [label = 'Sessions from Jan. 2017 to Sept. 2024 \n (n = 91,896,297)']
    C [label = 'Sessions after initial screening (n = 86,774,937)']
    B [label = 'Sessions excluded: missing demographic \n or blood pressure information (n = 5,121,360)']
    E [label = 'Sessions with hypertension diagnosis (n = 1,356,654)']
    D [label = 'Sessions excluded: no hypertension diagnosis (n = 85,418,283)']
    G [label = 'Analytic dataset (n = 1,270,485)']
    F [label = 'Sessions excluded: duplicates (n = 86,169)']
    
    # Define edges
    A -> B
    A -> C
    C -> D
    C -> E
    E -> F
    E -> G
  }
  ")
  
  svg_code <- export_svg(diag)
  rsvg_png(charToRaw(svg_code), file = paste0(filename, ".png"))
  rsvg_pdf(charToRaw(svg_code), file = paste0(filename, ".pdf"))
  return(diag)
}

plot_pursuant_nobs      <- function(){
  dt   <- fread( here("data", "processed", "high_quality_dt_after_deduplication_w_cov_Sept24.csv"),
                 colClasses = list(character = "FIPS"))
  
  YEAR_RANGES <- unique(dt$year_range) |> sort()
  
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
  
  return(fig_pursuant_nobs)
}

# Facet wrap county with state
plot_pursuant_estimates <- function(stage = "stage2", outcome = "prevalence", level = "state", breaks = "quantile", range = "four", min = 20, mid = 50, max = 85, var = "mean",
                                    one_range = "2023-2024"){

  # palette <- ifelse(outcome %in% c("prevalence", "awareness-marginal"), "YlOrRd", "RdYlGn")
  palette <- "YlGnBu"
  high_col <- ifelse(outcome %in% c("prevalence", "awareness-marginal"), "red", "green")

  # Year range
  if(range == "four"){
    year_ranges <- paste(c("2017-2020", "2021-2024"))
  } else if(range == "two"){
    year_ranges <- paste(c("2017-2018",
                           "2019-2020",
                           "2021-2022",
                           "2023-2024"))
  }else if (range == "one"){
    year_ranges <- paste0(one_range)
    
  }

  # Geographic level
  if(level == "state"){
    dt <- rbindlist(lapply(year_ranges, function(yr){
      name <- paste(yr, outcome, stage, sep = "_")
      fread(here("results", "estimates",
                 paste0(name, "_state.csv")))[, year_range := yr]
    }))
  } else if(level == "county"){
    dt <- rbindlist(lapply(year_ranges, function(yr){
      name <- paste(yr, outcome, stage, sep = "_")
      fread(here("results", "estimates",
                 paste0(name, "_county.csv")), colClasses = list(character = "FIPS"))[, year_range := yr]
    }))
  }else if(level=="brfss-county"){
    
    # dt <- fread(here("data", "raw", "CDC_PLACES.csv"), colClasses = list(character = "LocationID")) |>
    dt <- tryCatch({fread(here("data", "raw", "CDC_PLACES.csv"), colClasses = list(character = "LocationID")) |>
        dplyr::filter(LocationID != "59") |>
        dplyr::filter(MeasureId == "BPHIGH") |>
        dplyr::filter(Measure == "High blood pressure among adults") |>
        dplyr::select(LocationID, LocationName, StateAbbr, Data_Value_Type, Data_Value, Low_Confidence_Limit, High_Confidence_Limit) |>
        dplyr::rename(FIPS = LocationID, 
                      state = StateAbbr,
                      mean = Data_Value,
                      lower = Low_Confidence_Limit,
                      upper = High_Confidence_Limit) |>
        dplyr::arrange(FIPS, Data_Value_Type) |>
        dplyr::filter(Data_Value_Type == "Age-adjusted prevalence")},
        error = function(e){
          readRDS(paste0(path_spatial_kiosks_folder,"/working/cleaned/skbr01_high blood pressure and obesity from cdc places.RDS")) |>
            dplyr::filter(locationid != "59") |>
            dplyr::filter(measureid == "BPHIGH") |>
            # dplyr::filter(measure == "High blood pressure among adults") |>
            dplyr::select(locationid, locationname, stateabbr, data_value_type, data_value, low_confidence_limit, high_confidence_limit) |>
            dplyr::rename(FIPS = locationid, 
                          state = stateabbr,
                          # cdc_mean = Data_Value,
                          mean = data_value,
                          lower = low_confidence_limit,
                          upper = high_confidence_limit) |>
            mutate(mean = as.numeric(mean),
                   lower = as.numeric(lower),
                   upper = as.numeric(upper)) |>
            dplyr::arrange(FIPS, data_value_type) |>
            dplyr::filter(data_value_type == "Age-adjusted prevalence") %>% 
            data.table()
        })
    
  }

  # Adjust the weird ones where didn't become percentage
  dt[, mean := ifelse(mean < 1, mean*100, mean)]
  dt[, lower := ifelse(lower <1, lower*100, lower)]
  dt[, upper := ifelse(upper <1, upper*100,upper)]
  dt[, se := (upper - mean) / 1.96]

  # Breaks: quantiles, regular intervals, or continuous from 0 to 100. Lastly, custom breaks
  # if(breaks == "quantile"){
  #   custom_colors <- brewer.pal(5, palette)
  #   QUANTILES <- stats::quantile(dt$mean, seq(0, 1, .2),na.rm=TRUE)
  #   QUANTILES[1] <- QUANTILES[1] - 0.001
  #   BREAKS <- QUANTILES
  #   print(BREAKS)
  # } else if(breaks == "regular"){
  #   # We want these to be the same as those we will use for counties
  #   custom_colors <- brewer.pal(5, palette)
  #   MIN <- min(dt$mean)-0.001
  #   MAX <- max(dt$mean)
  #   BREAKS <- seq(MIN, MAX, length.out = 6)
  # } else if(breaks == "continuous"){
  # 
  # } else{
  # 
  # }

  if(level == "state"){
    merge_vars <- c("STUSPS" = "state", "year_range")
  } else if(level %in% c("county")){
    merge_vars <- c("GEOID" = "FIPS", "year_range")
  } else {
    # Added for brfss-county
    merge_vars <- c("GEOID" = "FIPS")
  }

  boundary_col <- ifelse(level == "state", "black", NA)

  # dt[, hbp_groups := cut(mean, breaks = BREAKS)]

  if(level %in% c("county","brfss-county")){
    # Added for brfss-county
    boundaries <- readRDS(here("data", "reference", paste0("county_boundaries_2022.rds")))
    state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
  }else{
    boundaries <- readRDS(here("data", "reference", paste0(level, "_boundaries_2022.rds")))
  }

  
  if(level %in% c("county","state")){
    size       <- nrow(boundaries)
    boundaries <- boundaries |>
      slice(rep(1:n(), length(year_ranges))) |>
      mutate(year_range = rep(year_ranges, each = size)) |>
      left_join(dt, by = merge_vars)
  }else{
    # Added for brfss-county
    boundaries <- boundaries |>
      left_join(dt, by = merge_vars)
  }
  
  
  # Actual plot code
  # plt <- ggplot() +
  #     geom_sf(data=boundaries,col=boundary_col,aes(fill=hbp_groups))  +
  #     scale_fill_manual(values = custom_colors) +
  #     theme_map +
  #     facet_wrap(~year_range) +
  #     labs(fill = paste0("Proportion (%)")) +
  #     ggtitle(tools::toTitleCase(paste0(stage, " ", outcome, " (", breaks, ")")) )
  
  if(outcome == "awareness-marginal" & var == "mean"){
    plt <- ggplot() +
        geom_sf(data=boundaries,col=boundary_col,aes(fill=mean))  +
        # scale_fill_distiller(palette = palette, direction = -1, limits = c(min, max), breaks = c(25,35,45,55)) +
        scale_fill_distiller(palette = palette, direction = 1, limits = c(min, max), breaks = breaks) +
        theme_map +
        labs(fill = paste0("Proportion (%)")) 
  } else if(outcome == "prevalence" & var == "mean"){
    plt <- ggplot() +
        geom_sf(data=boundaries,col=boundary_col,aes(fill=get(var)))  +
        scale_fill_distiller(palette = palette, direction = 1, limits = c(min, max), breaks = breaks) +
        theme_map  +
        labs(fill = paste0("Proportion (%)")) 
  } else if( var == "mean"){
    print(outcome)
    print(var)
    plt <- ggplot() +
        geom_sf(data=boundaries,col=boundary_col,aes(fill=get(var)))  +
        scale_fill_distiller(palette = palette, direction = 1,limits = c(min, max), breaks = breaks) +
        theme_map +
        labs(fill = paste0("Proportion (%)")) 
  } else if(var == "se"){
    plt <- ggplot() +
        geom_sf(data=boundaries,col=boundary_col,aes(fill=get(var)))  +
        #scale_fill_continuous(limits = c(min, max)) +
        theme_map +
        labs(fill = paste0("Standard Error (%)")) 
  }
  
  if(range %in% c("four","two")){
    plt <- plt +
      facet_wrap(~year_range)
    
  }

  if(level %in% c("county","brfss-county")){
    plt <- plt + geom_sf(data=state_boundaries,col="black",fill=NA)
  }

  # Unify the EPSG
  plt <- plt + coord_sf(crs = 5070, datum = NA)

  return(plt)
}

plot_brfss2021_scatter <- function(){
  # Read in CDC Places data
  cdc <- tryCatch({fread(here("data", "raw", "CDC_PLACES.csv"), colClasses = list(character = "LocationID")) |>
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
    dplyr::filter(Data_Value_Type == "Age-adjusted prevalence")},
    error = function(e){
      readRDS(paste0(path_spatial_kiosks_folder,"/working/cleaned/skbr01_high blood pressure and obesity from cdc places.RDS")) |>
        dplyr::filter(locationid != "59") |>
        dplyr::filter(measureid == "BPHIGH") |>
        # dplyr::filter(measure == "High blood pressure among adults") |>
        dplyr::select(locationid, locationname, stateabbr, data_value_type, data_value, low_confidence_limit, high_confidence_limit) |>
        dplyr::rename(FIPS = locationid, 
                      state = stateabbr,
                      cdc_mean = data_value,
                      lower = low_confidence_limit,
                      upper = high_confidence_limit) |>
        mutate(cdc_mean = as.numeric(cdc_mean),
               lower = as.numeric(lower),
               upper = as.numeric(upper)) |>
        dplyr::arrange(FIPS, data_value_type) |>
        dplyr::filter(data_value_type == "Age-adjusted prevalence") %>% 
        as.data.table()
    })
  
  # Read in corresponding Pursuant estimates. BRFSS compare to Awareness.
  est <- here("results", "estimates", "2021-2022_awareness-marginal_stage1_county.csv") |> fread(colClasses = list(character = "FIPS")) |>
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
    coord_cartesian(xlim=c(20, 55), ylim = c(20, 55))
    ggtitle("Comparison of hypertension awareness")
  
  return(plt)
}

plot_pursuant_unadjust_vs_adjust_state <- function(stage = "stage2", outcome = "awareness-conditional", year_range_arg = "2021-2022"){
  model_name <- paste(year_range_arg, outcome, stage, sep = "_")
  
  # Model-based estimates
  model_est <- here("results", "estimates", paste0(model_name, "_state.csv")  ) |>
    fread() |>
    mutate(type = "Model-adjusted", 
           level = "State",
           mean = ifelse(mean<1, mean*100, mean),
           lower = ifelse(lower<1,lower*100, lower),
           upper = ifelse(upper<1,upper*100, upper))
  
  national_est <- here("results", "estimates", paste0(model_name, "_national.csv")  ) |>
    fread() |>
    mutate(type = "Model-adjusted", 
           level = "National", state = "National",
           mean = ifelse(mean<1, mean*100, mean),
           lower = ifelse(lower<1,lower*100, lower),
           upper = ifelse(upper<1,upper*100, upper))
  
  # Numerator variable
  num_var <- switch(outcome,
                    "prevalence" = paste0("hbp_", stage),
                    "awareness-marginal" = paste0("hbp_diagnosis"),
                    "awareness-conditional" = paste0("hbp_diagnosis"),
                    "controlled" = paste0("!hbp_bp_", stage),
                    NA) 
  
  # Denominator variable
  denom_var <- switch(outcome,
                    "awareness-conditional" = paste0("hbp_", stage),
                    "controlled" = paste0("hbp_diagnosis & hbp_", stage),
                    "1 == 1") 
  
  # Raw estimates
  data_est <- here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv") |> 
                  fread(colClasses = list(character = "FIPS")) |>
                  filter(.data$year_range == year_range_arg & !!rlang::parse_expr(denom_var)) |>
                  group_by(state) |>
                  summarise(mean = mean(!!rlang::parse_expr(num_var)), n = n()) |>
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
    ggtitle(model_name) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100))
  
  return(plt)
} 

plot_barplot_by_gender <- function(stage = "stage2", outcome = "prevalence", year_range = "2021-2022", demo_var = "ethnicity", include_title = TRUE){
  if(year_range == "All"){
    year_range <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024")
  }
  
  dt <- lapply(year_range, function(year){
    model_name <- paste(year, outcome, stage, sep = "_")
    if(demo_var == "ethnicity"){
      types <- c("ethnicity", "ethnicity-gender")
    } else if(demo_var == "age_group"){
      types <- c("national", "gender", "age-gender", "age_group")
    }
    filenames  <- paste0(model_name, "_", types, ".csv")
    files      <- here("results", "estimates", filenames)
    
    dt <- lapply(files, fread) |> rbindlist(fill = TRUE)
    dt[, year_range := year]
    dt[, gender := ifelse(is.na(gender), "All", gender)]
    if(demo_var == "age_group"){
      dt[, age_group := ifelse(is.na(age_group), ">18", age_group)]
    }
    return(dt)
  }) |>
    rbindlist() |>
    mutate(gender = factor(gender, levels = c("All", "male", "female"), 
                           labels = c("All", "Men", "Women")),
           mean = ifelse(mean < 1, mean*100, mean),
           lower = ifelse(lower < 1, lower*100, lower),
           upper = ifelse(upper < 1, upper*100, upper))
  
  if(demo_var == "ethnicity"){
    dt <- dt |>
      mutate(ethnicity = factor(ethnicity, levels = c("white", "black", "hispanic", "asian", "other"), 
                     labels = c("NH White", "NH Black", "Hispanic", "NH Asian", "NH Other")))
  } 
  
  if(demo_var == "age_group"){
    dt <- dt |>
      mutate(age_group = factor(age_group, levels = c(">18", "18-19", "20-44", "45-64", "65plus"),
                                labels = c("18+", "18 to <20", "20 to <45", "45 to <65", "65+")))    
  }
  
  barchart <- 
    ggplot(dt, aes(gender, mean, fill = !!rlang::parse_expr(demo_var))) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_point(position = position_dodge(0.9), size = 0.5) +
    geom_errorbar(width=0.25,
      aes(
          ymin = lower,
          ymax = upper), position = position_dodge(0.9)
    ) +
    ylab("Proportion (%)") + 
    xlab(NULL) + 
    theme_classic(base_size = 18) + 
    geom_text(aes(label=round(mean,1)), position=position_dodge(width=0.9), vjust=-2.5) +
    # geom_text(aes(label=round(mean,1)), position=position_dodge(width=0.9)) +
    theme(legend.position = "top") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
    facet_wrap(~year_range)
 
  if(demo_var == "ethnicity"){
    barchart <- barchart + 
      scale_fill_manual(name = "", 
                        values = c("NH White" = "#586891FF", 
                                   "NH Black" = "#8897A4FF", 
                                   "Hispanic" = "#B3A6A3FF",
                                   "NH Asian" = "#2B152CFF",
                                   "NH Other" = "#F1F3F2FF")) 
  } else if(demo_var == "age_group"){
    barchart <- barchart + 
    scale_fill_manual(name = "", 
                      values = c("18+" = "#3182bd",
                                 "18 to <20" =  "#63589FFF", 
                                 "20 to <45" =  "#D1AFE8FF", 
                                 "45 to <65" = "#F3E0F7FF", 
                                 "65+" = "grey")) 
  }
   
  if(include_title){
   barchart <- barchart + 
      ggtitle(paste0(stage, " ", outcome))
  }
  return(barchart)
}

# Figure S3 ---------------------------------------------------------------

plot_bp_over_time <- function(){
  full <- open_dataset(here("data", "raw", "emory-limited-data-set-export-Sept24"), format = "parquet") |> 
      mutate(year = substr(session_received_utc, 1, 4),
             bp_systolic = as.numeric(bp_systolic),
             bp_diastolic = as.numeric(bp_diastolic)) 
    
  full.summary <- full |>
    group_by(year) |>
    summarise(`Avg. Systolic BP (mmHg)` = mean(bp_systolic),
              `Avg. Diastolic BP (mmHg)` = mean(bp_diastolic),
              `Percent of Observations (%)` = n() / nrow(full) * 100) |>
    collect()|>
    melt(id.vars = "year") |>
    mutate(type = "Full Data (N = 91.9 million)")
    dt <- fread(here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv"), 
                colClasses = list(character = "FIPS"))
  
  # Did the higher blood pressure apply to all sessions? 
  dt.summary <- dt[, .(`Avg. Systolic BP (mmHg)` = mean(sbp), 
              `Avg. Diastolic BP (mmHg)` = mean(dbp),
              `Percent of Observations (%)` = 100 * .N / nrow(dt)), year]
              # hbp_diagnosis = mean(hbp_diagnosis),
              # hbp_stage1 = mean(hbp_stage1),
              # hbp_stage2 = mean(hbp_stage2),
              # hbp_bp_stage1 = mean(hbp_bp_stage1),
              # hbp_bp_stage2 = mean(hbp_bp_stage2)), year][order(year)]
  
  dt.summary <- dt.summary |>
    melt(id.vars = "year") |>
    mutate(type = "Subset (N = 1.27 million)")
  
  plot.dt <- rbind(full.summary, dt.summary)
  
  plt <- ggplot(plot.dt[,], aes(year, value)) + 
    geom_point(aes(col = type)) + 
    geom_line(aes(group = type, col = type)) + 
    facet_wrap(~variable, scales = "free") + 
    theme_bw() + 
    theme(legend.position = "bottom") 
  
 return(plt) 
}

# Figure S4 ---------------------------------------------------------------
plot_local_moran_I <- function(significance = FALSE){
  # Cluster analysis of BRFSS 2021 and Pursuant Awareness -------------------
  states <- readRDS(here("data/reference/state_boundaries_2022.rds"))
  cdc    <- data.table::fread(here("data", "raw", "CDC_PLACES.csv"), colClasses = list(character = "LocationID")) |>
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
  est <- here("results", "estimates", "2021-2022_awareness-marginal_stage1_county.csv") |> 
    data.table::fread(colClasses = list(character = "FIPS")) |>
    dplyr::left_join(cdc[, .(FIPS, cdc_mean)]) 
  
  counties <- readRDS(here("data", "reference", paste0("county_boundaries_2022.rds"))) |>
    dplyr::left_join(est, by = c(GEOID = "FIPS")) |>
    dplyr::filter(state != "FL")
  
  # Step 2: Create a neighbors list and spatial weights
  # Create neighbors list using queen contiguity
  nb <- poly2nb(counties) 
  
  # Create spatial weights (style = "W" for row-standardized weights)
  weights <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Step 3: Compute Local Moran's I
  # Replace 'outcome_column' with the name of your variable (e.g., 'prevalence')
  local_moran <- localmoran(counties$mean, weights, zero.policy = TRUE)
  local_moran.brfss <- localmoran(counties$cdc_mean, weights, zero.policy = TRUE)
  
  # Step 4: Attach results to the sf object
  counties$Local_Moran_I.pursuant <- local_moran[, "Ii"]
  counties$P_Value.pursuant <- local_moran[, "Pr(z != E(Ii))"]
  counties$Local_Moran_I.brfss <- local_moran.brfss[, "Ii"]
  counties$P_Value.brfss <- local_moran.brfss[, "Pr(z != E(Ii))"]
  if(significance){
    counties$quadr.pursuant <- ifelse(counties$P_Value.pursuant < 0.05, attributes(local_moran)$quadr[, "mean"], "Non-significant")
    counties$quadr.brfss <- ifelse(counties$P_Value.brfss < 0.05, attributes(local_moran.brfss)$quadr[, "mean"], "Non-significant")
  } else{
    counties$quadr.pursuant <- attributes(local_moran)$quadr[, "mean"]
    counties$quadr.brfss <- attributes(local_moran.brfss)$quadr[, "mean"]
  }
  
  # ggplot(counties, aes(Local_Moran_I.pursuant, Local_Moran_I.brfss)) + 
  #   geom_point(alpha = 0.3) + 
  #   geom_abline(slope = 1, intercept = 0) + 
  #   coord_equal()
  
  # cor(counties$Local_Moran_I.brfss, counties$Local_Moran_I.pursuant)
  
  # Recall: Of all hotspots, how many were predicted? 
  # counties |>
  #   dplyr::filter(quadr.brfss == "1") |>
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # 
  # counties |>
  #   dplyr::filter(quadr.brfss %in% c(1,4)) |>
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # counties |>
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # counties |>
  #   dplyr::filter(quadr.brfss == "4") |> 
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # 
  # counties |>
  #   dplyr::filter(quadr.brfss == "Non-significant") |> 
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # 
  # # Precision: Of the hotspots predicted, how many are actually hotspots 
  # counties |>
  #   dplyr::filter(quadr.pursuant == "1") |> 
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # 
  # counties |>
  #   dplyr::filter(quadr.pursuant %in% c(1,4)) |>
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # counties |>
  #   dplyr::filter(quadr.pursuant == "4") |> 
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  # 
  # counties |>
  #   dplyr::filter(quadr.pursuant == "Non-significant") |> 
  #   dplyr::summarise(mean(quadr.pursuant == quadr.brfss, na.rm = TRUE), sum(!is.na(quadr.pursuant) & !is.na(quadr.brfss)))
  
  # Step 5: Visualize results
  # Create a choropleth map of Local Moran's I
  quadr.pursuant.plt <- tm_shape(counties) +
    tm_fill("quadr.pursuant", title = "Quadrant") +
    tm_borders(lwd = 0.5, col = "gray", alpha = 0.9) +
    tm_shape(states) + 
    tm_borders(lwd = 1, col = "black") +
    tm_layout(title = "Pursuant 2021-2022")
  quadr.brfss.plt <- tm_shape(counties) +
    tm_fill("quadr.brfss", title = "Quadrant") +
    tm_borders(lwd = 0.5, col = "gray", alpha = 0.9) +
    tm_shape(states) + 
    tm_borders(lwd = 1, col = "black") +
    tm_layout(title = "BRFSS 2021")
  
  quadr.plt <- tmap_arrange(quadr.brfss.plt, quadr.pursuant.plt, ncol = 2, sync = TRUE)
  
  # In counties that are high-high in BRFSS, what % are identified in Pursuant?
  # In counties that are high-low in BRFSS, what % are identified in Pursuant? 
  
  # Optionally, map p-values to identify significant clusters
  # tm_shape(counties) +
  #   tm_fill("Local_Moran_I.brfss", style = "quantile", palette = "viridis", title = "Local Moran's I") +
  #   tm_borders() +
  #   tm_layout(title = "Local Moran's I for High Blood Pressure Prevalence")
  # 
  # # Optionally, map p-values to identify significant clusters
  # tm_shape(counties) +
  #   tm_fill("P_Value.brfss", style = "fixed", breaks = c(0, 0.05, 0.1, 1), 
  #           palette = c("red", "orange", "lightgrey"), 
  #           title = "P-Value (Significance)") +
  #   tm_borders() +
  #   tm_layout(title = "Significant Clusters (P-Value)")
  
  return(quadr.plt)
}




