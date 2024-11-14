library(data.table)
library(dplyr)
library(here)
library(lme4)
library(matrixStats)
library(merTools)
library(splines)

invlogit <- function(x){exp(x) / (1 + exp(x))}
  # If just need point estimate (county and state maps)
  # if(!compute.PI){
  #   ps[, pred.H := predict(fit.H,
  #                        re.form = NULL,
  #                        newdata = ps,
  #                        allow.new.levels = TRUE,
  #                        type = "response")]
  #   ps[, pred.D := predict(fit.D,
  #                        re.form = NULL,
  #                        newdata = ps,
  #                        allow.new.levels = TRUE,
  #                        type = "response")]
  #   ps[, pred.C := predict(fit.C,
  #                        re.form = NULL,
  #                        newdata = ps,
  #                        allow.new.levels = TRUE,
  #                        type = "response")]
  #   dt_weighted_means <- ps[, .(est = 100 * weighted.mean(pred, n)), by = group.vars]
  # }

#' @param level geographic level at which to generate estimates
#' @param YR 2-yr range 
#' @param n.sims number of simulations to compute prediction intervals
#' @param group.vars character vector of the grouping variables to compute post-stratified estimates over
poststratification <- function(YR = "2017-2018", 
                               stage = "stage2",
                               group.vars = "FIPS", 
                               n.sims = 500, 
                               seed = 123, ...){ 
  # Model names for each of the outcomes
  model_name.H <- paste(YR, "prevalence_marginal", stage, sep = "_")
  model_name.Dm <- paste(YR, "awareness_marginal", stage, sep = "_")
  model_name.Dc <- paste(YR, "awareness_conditional", stage, sep = "_")
  model_name.C <- paste(YR, "controlled_conditional", stage, sep = "_")
  
  # Fit object for each of the outcomes
  fit.H <- readRDS(here("results", "models", paste0("glmer_", model_name.H, ".rds")))
  fit.Dm <- readRDS(here("results", "models", paste0("glmer_", model_name.Dm, ".rds")))
  fit.Dc <- readRDS(here("results", "models", paste0("glmer_", model_name.Dc, ".rds")))
  fit.C <- readRDS(here("results", "models", paste0("glmer_", model_name.C, ".rds")))
  
  # Post-stratification table
  ps  <- fread( here("data/reference/poststratification_table.csv"), 
                         colClasses = list(character = c("FIPS", "state_code")))
  
  # This step can take a long time. Cache.
  # If need prediction intervals (Tables at the national level. State level for supplement)
  # if(compute.PI){
    # Filename for prediction simulations
    filename.H <- paste0("sims_PI_",n.sims, "_", model_name.H, ".rds")
    filepath.H <- here("results", "draws", filename.H)
    
    filename.Dm <- paste0("sims_PI_",n.sims, "_", model_name.Dm, ".rds")
    filepath.Dm <- here("results", "draws", filename.Dm)
    
    filename.Dc <- paste0("sims_PI_",n.sims, "_", model_name.Dc, ".rds")
    filepath.Dc <- here("results", "draws", filename.Dc)
    
    filename.C <- paste0("sims_PI_",n.sims, "_", model_name.C, ".rds")
    filepath.C <- here("results", "draws", filename.C)
      
    # Predict draws from original model
    message("Draws for prevalence")
    if(file.exists(filepath.H)){
      preds.H <- readRDS(filepath.H)
    } else{
      preds.H <- merTools::predictInterval(fit.H, 
                                           newdata = ps, 
                                           n.sims = n.sims,
                                           type = c("probability"),
                                           returnSims = TRUE, 
                                           seed = seed, ...)
      saveRDS(preds.H, file = filepath.H)
    }
    
    message("Draws for awareness-marginal")
    if(file.exists(filepath.Dm)){
      preds.Dm <- readRDS(filepath.Dm)
    } else{
      preds.Dm <- merTools::predictInterval(fit.Dm, 
                                             newdata = ps, 
                                             n.sims = n.sims,
                                             type = c("probability"),
                                             returnSims = TRUE, 
                                             seed = seed, ...)
      saveRDS(preds.Dm, file = filepath.Dm)
    }
    
    message("Draws for awareness-conditional")
    if(file.exists(filepath.Dc)){
      preds.Dc <- readRDS(filepath.Dc)
    } else{
      preds.Dc <- merTools::predictInterval(fit.Dc, 
                                             newdata = ps, 
                                             n.sims = n.sims,
                                             type = c("probability"),
                                             returnSims = TRUE, 
                                             seed = seed, ...)
      saveRDS(preds.Dc, file = filepath.Dc)
    }
    
    message("Draws for controlled")
    if(file.exists(filepath.C)){
      preds.C <- readRDS(filepath.C)
    } else{
      preds.C <- merTools::predictInterval(fit.C, 
                                             newdata = ps, 
                                             n.sims = n.sims,
                                             type = c("probability"),
                                             returnSims = TRUE, 
                                             seed = seed, ...)
      saveRDS(preds.C, file = filepath.C)
    }
    
  # Transform simulation results into manageable format
  simResults.H <- attributes(preds.H)$sim.results |> 
    invlogit() |>
    data.table()
  simResults.Dm <- attributes(preds.Dm)$sim.results |> 
    invlogit() |>
    data.table()
  simResults.Dc <- attributes(preds.Dc)$sim.results |> 
    invlogit() |>
    data.table()
  simResults.C <- attributes(preds.C)$sim.results |> 
    invlogit() |>
    data.table()
  
  # Compute marginal probabilities for awareness-conditional and controlled
  simResults.DcH  <- simResults.Dc * simResults.H
  simResults.CDcH <- simResults.C * simResults.Dc * simResults.H
  
  # Merge to PS table dataset
  dt.H    <- cbind(ps, data.table(simResults.H))
  dt.Dm   <- cbind(ps, data.table(simResults.Dm))
  dt.DcH  <- cbind(ps, data.table(simResults.DcH))
  dt.CDcH <- cbind(ps, data.table(simResults.CDcH))
  
  # Perform poststratification to the desired level
  dt_weighted_means.H <- dt.H[, lapply(.SD, function(x) 100 * weighted.mean(x, n)), 
                          by = group.vars, .SDcols = paste0("V", 1:n.sims)]
  dt_weighted_means.Dm <- dt.Dm[, lapply(.SD, function(x) 100 * weighted.mean(x, n)), 
                          by = group.vars, .SDcols = paste0("V", 1:n.sims)]
  dt_weighted_means.DcH <- dt.DcH[, lapply(.SD, function(x) 100 * weighted.mean(x, n)), 
                          by = group.vars, .SDcols = paste0("V", 1:n.sims)]
  dt_weighted_means.CDcH <- dt.CDcH[, lapply(.SD, function(x) 100 * weighted.mean(x, n)), 
                          by = group.vars, .SDcols = paste0("V", 1:n.sims)]
  
  # Get in matrix form
  dt_matrix.H <- dt_weighted_means.H[, paste0("V", 1:n.sims), with = FALSE] |>
    as.matrix()
  dt_matrix.Dm <- dt_weighted_means.Dm[, paste0("V", 1:n.sims), with = FALSE] |>
    as.matrix()
  dt_matrix.DcH <- dt_weighted_means.DcH[, paste0("V", 1:n.sims), with = FALSE] |>
    as.matrix()
  dt_matrix.CDcH <- dt_weighted_means.CDcH[, paste0("V", 1:n.sims), with = FALSE] |>
    as.matrix()
  
  # Transform back to [D|H] and [C|D,H] conditional probabilities
  dt_matrix.Dc <- dt_matrix.DcH / dt_matrix.H
  dt_matrix.C <- dt_matrix.CDcH / dt_matrix.DcH
  
  # Calculate mean, lower, and upper for the Post stratified [H], [D|H], and [C|D,H]
  dt_weighted_means.H[, `:=`(mean = rowMeans(dt_matrix.H),
                           median = rowMedians(dt_matrix.H),
                           lower = rowQuantiles(dt_matrix.H, probs = 0.025),
                           upper = rowQuantiles(dt_matrix.H, probs = 0.975))][, paste0("V", 1:n.sims) := NULL]
  dt_weighted_means.Dm[, `:=`(mean = rowMeans(dt_matrix.Dm),
                           median = rowMedians(dt_matrix.Dm),
                           lower = rowQuantiles(dt_matrix.Dm, probs = 0.025),
                           upper = rowQuantiles(dt_matrix.Dm, probs = 0.975))][, paste0("V", 1:n.sims) := NULL]
  dt_weighted_means.DcH[, `:=`(mean = rowMeans(dt_matrix.Dc),
                           median = rowMedians(dt_matrix.Dc),
                           lower = rowQuantiles(dt_matrix.Dc, probs = 0.025),
                           upper = rowQuantiles(dt_matrix.Dc, probs = 0.975))][, paste0("V", 1:n.sims) := NULL]
  dt_weighted_means.CDcH[, `:=`(mean = rowMeans(dt_matrix.C),
                           median = rowMedians(dt_matrix.C),
                           lower = rowQuantiles(dt_matrix.C, probs = 0.025),
                           upper = rowQuantiles(dt_matrix.C, probs = 0.975))][, paste0("V", 1:n.sims) := NULL]
  
  return(list(prevalence=dt_weighted_means.H, 
              `awareness-marginal`=dt_weighted_means.Dm,
              `awareness-conditional`=dt_weighted_means.DcH,
              controlled=dt_weighted_means.CDcH))
}
