library(data.table)
library(dplyr)
library(here)
library(lme4)
library(matrixStats)
library(merTools)

invlogit <- function(x){exp(x) / (1 + exp(x))}

#' @param level geographic level at which to generate estimates
#' @param YR 2-yr range 
#' @param n.sims number of simulations to compute prediction intervals
#' @param group.vars character vector of the grouping variables to compute post-stratified estimates over
poststratification <- function(YR = "2017-2018", 
                               outcome = "prevalence",
                               stage = "stage2",
                               group.vars = "FIPS", 
                               compute.PI = TRUE,
                               n.sims = 500, 
                               seed = 123, ...){ 
  model_name <- paste(YR, outcome, stage, sep = "_")
  
  # Fixed effects for demographic variables
  fit <- readRDS(here("results", "models", paste0("glmer_", model_name, ".rds")))
  ps  <- fread( here("data/reference/poststratification_table.csv"), 
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
    filepath <- here("results", "draws", filename)
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