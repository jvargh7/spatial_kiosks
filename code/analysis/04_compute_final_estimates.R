library(here)
library(data.table)

source(here("R", "compute_poststratification_estimates.R"))

if(interactive()){
  YEAR_RANGE <- "2017-2018"
  OUTCOME    <- "prevalence"
  STAGE      <- "stage2"
  LEVEL      <- "national"
} else{
  args     <- commandArgs(trailingOnly = TRUE)
  ID       <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  taskmap  <- fread(here("data", "taskmap", "taskmap_final_estimates.csv"))
  YEAR     <- taskmap[ID, YEARS]
  OUTCOME  <- taskmap[ID, OUTCOMES]
  STAGE    <- taskmap[ID, STAGES]
  LEVEL    <- taskmap[ID, LEVEL]
}

# Map data ----------------------------------------------------------------
model_name <- paste(YEAR, OUTCOME, STAGE, LEVEL, sep = "_")
message("Processing ", model_name)

# Define the grouping variable based on LEVEL.
group.vars <- switch(LEVEL,
                     "county" = c("FIPS", "state"),
                     "national" = c(),
                     "ethnicity-gender" = c("ethnicity", "gender"),
                     "age-gender" = c("age_group", "gender"),
                     LEVEL) # Default case if LEVEL does not match any of the specified conditions

estimate <- poststratification(YR = YEAR, 
                               outcome = OUTCOME, 
                               stage = STAGE, 
                               group.vars = group.vars, 
                               compute.PI = TRUE)

fwrite(estimate, file = here("results", "estimates", paste0(model_name, ".csv")))

message("DONE")