library(data.table)
library(dplyr)
library(here)
library(lme4)
library(matrixStats)
library(merTools)

# theme_map <- theme_bw(base_size = 14) +
#   theme(legend.position = "bottom", 
#         legend.text = element_text(size = 14))

invlogit <- function(x){exp(x) / (1 + exp(x))}

#' @param level geographic level at which to generate estimates
#' @param YR 2-yr range 
#' @param n.sims number of simulations to compute prediction intervals
#' @param group.vars character vector of the grouping variables to compute post-stratified estimates over
poststratification <- function(YR = "2017-2018", 
                               outcome = "prevalence",
                               stage = "stage2",
                               group.vars = "FIPS", 
                               compute.PI = FALSE,
                               n.sims = 1000, 
                               seed = 123, ...){ 
  model_name <- paste(YR, outcome, stage, sep = "_")
  
  fit <- readRDS(here("outputs", paste0("glmer_", model_name, ".rds")))
  ps  <- fread( here("data/poststratification_table.csv"), 
                         colClasses = list(character = c("FIPS", "state_code")))
  
  # If just need point estimate (county and state maps)
  if(!compute.PI){
    ps[, pred := predict(fit,
                         re.form = NULL,
                         newdata = ps,
                         allow.new.levels = TRUE,
                         type = "response")]
    dt_weighted_means <- ps[, .(est = 100 * weighted.mean(pred, n)), by = group.vars]
  }
  
  # This step can take a long time. Cache.
  # If need prediction intervals (Tables at the national level. State level for supplement)
  if(compute.PI){
    
    # Filename for prediction simulations
    filename <- paste0("sims_PI_",n.sims, "_", model_name, ".rds")
    filepath <- here("outputs", filename)
    if(file.exists(filepath)){
      preds <- readRDS(filepath)
    } else{
      preds <- merTools::predictInterval(fit, 
                                         newdata = ps, 
                                         n.sims = n.sims,
                                         type = c("probability"),
                                         returnSims = TRUE, 
                                         seed = seed, ...)
      saveRDS(preds, file = filepath)
    }
    
  simResults <- attributes(preds)$sim.results |> invlogit() |>
    data.table()
  
  dt <- cbind(ps, data.table(simResults))
  
  dt_weighted_means <- dt[, lapply(.SD, function(x) 100 * weighted.mean(x, n)), 
                          by = group.vars, .SDcols = paste0("V", 1:n.sims)]
  
  dt_matrix <- dt_weighted_means[, paste0("V", 1:n.sims), with = FALSE] |>
    as.matrix()
  
  dt_weighted_means[, `:=`(mean = rowMeans(dt_matrix),
                           median = rowMedians(dt_matrix),
                           lower = rowQuantiles(dt_matrix, probs = 0.025),
                           upper = rowQuantiles(dt_matrix, probs = 0.975))][, paste0("V", 1:n.sims) := NULL]
  }
  
  return(dt_weighted_means)
}



# dt_list <- vector(mode = "list", 4)
# YEAR_RANGES <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024")
# 
# for(YR in YEAR_RANGES){
#   
#   ps_copy       <- copy(ps)
#   dt_list[[YR]] <-  ps_copy[, pred := predict(fit, ps, allow.new.levels = TRUE, type = "response")][, year_range := YR]
#   
# }
# 
# dt <- rbindlist(dt_list)
# if(level == "county"){
#   dt <- dt[, .(est = 100 * weighted.mean(pred, n) ), by = .(FIPS, year_range)]
# }
# if(level == "state"){
#  dt <- dt[, .(est = 100 * weighted.mean(pred, n) ), by = .(state, year_range)]
# }
# 
# QUANTILES <- quantile(dt$est, seq(0, 1, .2))
# QUANTILES[1] <- QUANTILES[1] - 0.001
# 
# 
# dt[, hbp_groups := cut(est, breaks = QUANTILES)]
# 
# #custom_colors <- colorRampPalette(c("white", "red"))(5)
# custom_colors <- brewer.pal(5, "YlOrRd")
# 
# # Read in previously saved `sf` objects for county/state boundaries
# county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) |>
#   left_join(dt, by = c("GEOID" = "FIPS"))
# state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds")) |>
#   left_join(dt, by = c("STUSPS" = "state"))
# 
# pdf(file = here("county_estimates.pdf"), width = 11, height = 8.5)
# for(YR in YEAR_RANGES){
#   fig_pursuant_hbp <- ggplot() +
#       geom_sf(data=county_boundaries[county_boundaries$year_range == YR,],aes(fill = hbp_groups))  +
#       geom_sf(data=state_boundaries,col="black",fill=NA)  +
#       coord_sf(crs = 5070, datum = NA) +
#     scale_fill_manual(values = custom_colors) + 
#       theme_map +
#     ggtitle(YR)
#   
#   print(fig_pursuant_hbp)
# }
# dev.off()
# 
# pdf(file = here("state_estimates.pdf"), width = 11, height = 8.5)
# for(YR in YEAR_RANGES){
#   fig_pursuant_hbp <- ggplot() +
#       geom_sf(data=state_boundaries[state_boundaries$year_range == YR,],aes(fill = hbp_groups))  +
#       # geom_sf(data=state_boundaries,col="black",fill=NA)  +
#       coord_sf(crs = 5070, datum = NA) +
#     scale_fill_manual(values = custom_colors) + 
#       theme_map +
#     ggtitle(YR)
#   
#   print(fig_pursuant_hbp)
# }
# dev.off()
# 
# 
# 
# 
# YR <- "2023-2024"
# 
# fit <- readRDS(here("outputs", paste0("glmer_", YR, "_prevalence_stage2.rds")))
# 
# ps <- fread(here("data/poststratification_table.csv"), 
#             colClasses = list(character = c("FIPS", "state_code")))
# 
# ps[, pred := predict(fit, ps, allow.new.levels = TRUE, type = "response")]
# 
# if(level == "county"){
#   dt <- ps[, .(est = 100 * weighted.mean(pred, n) ), by = FIPS]
# }
# if(level == "state"){
#  dt <- ps[, weighted.mean(pred, n), by = state]
# }
# 
# dt[, hbp_groups := cut(est, breaks = quantile(est, seq(0,1,.2)))]
# 
# #custom_colors <- colorRampPalette(c("white", "red"))(5)
# custom_colors <- brewer.pal(5, "YlOrRd")
# 
# # Read in previously saved `sf` objects for county/state boundaries
# county_boundaries <- readRDS(here("data", "reference", "county_boundaries_2022.rds")) |>
#   left_join(dt, by = c("GEOID" = "FIPS"))
# state_boundaries <- readRDS(here("data", "reference", "state_boundaries_2022.rds"))
# 
# fig_pursuant_hbp <- ggplot() +
#     geom_sf(data=county_boundaries,aes(fill = hbp_groups))  +
#     geom_sf(data=state_boundaries,col="black",fill=NA)  +
#     coord_sf(crs = 5070, datum = NA) +
#   scale_fill_manual(values = custom_colors) + 
#     theme_map
# 
# fig_pursuant_hbp
