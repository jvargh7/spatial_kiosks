library(here)
library(data.table)

source(here("R", "compute_poststratification_estimates.R"))

if(interactive()){
  YEAR_RANGE <- "2017-2018"
  OUTCOME    <- "prevalence"
  STAGE      <- "stage2"
  LEVEL     <- "national"
} else{
  args       <- commandArgs(trailingOnly = TRUE)
  ID         <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  taskmap    <- fread(here("data", "taskmap_all_final_estimates.csv"))
  YEAR <- taskmap[ID, YEARS]
  OUTCOME    <- taskmap[ID, OUTCOMES]
  STAGE      <- taskmap[ID, STAGES]
  LEVEL      <- taskmap[ID, LEVEL]
  # LEVEL    <- taskmap[ID, LEVEL]
}

# Map data ----------------------------------------------------------------
model_name <- paste(YEAR, OUTCOME, STAGE, LEVEL, sep = "_")
message("Processing ", model_name)
      
if(LEVEL == "county"){
    # message("national")
    national_estimate   <- poststratification(YR = YEAR, outcome = OUTCOME, stage = STAGE, group.vars = c("FIPS", "state"), compute.PI = TRUE)
    fwrite(national_estimate, file = here("results", paste0(model_name, "_fixed-demo.csv")))
} 

if(LEVEL == "national"){
    national_estimate   <- poststratification(YR = YEAR, outcome = OUTCOME, stage = STAGE, group.vars = c(), compute.PI = TRUE)
    fwrite(national_estimate, file = here("results", paste0(model_name, "_fixed-demo.csv")))
}

if(LEVEL == "ethnicity-gender"){
    national_estimate   <- poststratification(YR = YEAR, outcome = OUTCOME, stage = STAGE, group.vars = c("ethnicity", "gender"), compute.PI = TRUE)
    fwrite(national_estimate, file = here("results", paste0(model_name, "_fixed-demo.csv")))
}

if(LEVEL == "age-gender"){
    national_estimate   <- poststratification(YR = YEAR, outcome = OUTCOME, stage = STAGE, group.vars = c("age_group", "gender"), compute.PI = TRUE)
    fwrite(national_estimate, file = here("results", paste0(model_name, "_fixed-demo.csv")))
}

if( !(LEVEL %in% c("county", "national", "age-gender", "ethnicity-gender")) ){
    national_estimate   <- poststratification(YR = YEAR, outcome = OUTCOME, stage = STAGE, group.vars = LEVEL, compute.PI = TRUE)
    fwrite(national_estimate, file = here("results", paste0(model_name, "_fixed-demo.csv")))
}
message("DONE")
