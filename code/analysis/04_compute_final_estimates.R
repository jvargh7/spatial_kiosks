library(here)
library(data.table)

source(here("R", "compute_poststratification_estimates.R"))

if(interactive()){
  YEAR  <- "2017-2020"
  STAGE <- "stage2"
  LEVEL <- "state"
} else{
  args     <- commandArgs(trailingOnly = TRUE)
  ID       <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  taskmap  <- fread(here("data", "taskmap", "taskmap_final_estimates.csv"))
  YEAR     <- taskmap[ID, YEARS]
  STAGE    <- taskmap[ID, STAGES]
  LEVEL    <- taskmap[ID, LEVEL]
}

# Map data ----------------------------------------------------------------
model_name.H  <- paste(YEAR, "prevalence", STAGE, LEVEL, sep = "_")
model_name.Dm <- paste(YEAR, "awareness-marginal", STAGE, LEVEL, sep = "_")
model_name.Dc <- paste(YEAR, "awareness-conditional", STAGE, LEVEL, sep = "_")
model_name.C  <- paste(YEAR, "controlled", STAGE, LEVEL, sep = "_")

message("Processing ", model_name.H)

# Define the grouping variable based on LEVEL.
group.vars <- switch(LEVEL,
                     "county" = c("FIPS", "state"),
                     "national" = c(),
                     "ethnicity-gender" = c("ethnicity", "gender"),
                     "age-gender" = c("age_group", "gender"),
                     LEVEL) # Default case if LEVEL does not match any of the specified conditions

estimate <- poststratification(YR = YEAR, 
                               stage = STAGE, 
                               group.vars = group.vars, 
                               n.sims = 500)

fwrite(estimate$prevalence, file = here("results", "estimates", paste0(model_name.H, ".csv")))
fwrite(estimate$`awareness-marginal`, file = here("results", "estimates", paste0(model_name.Dm, ".csv")))
fwrite(estimate$`awareness-conditional`, file = here("results", "estimates", paste0(model_name.Dc, ".csv")))
fwrite(estimate$controlled, file = here("results", "estimates", paste0(model_name.C, ".csv")))

message("DONE")