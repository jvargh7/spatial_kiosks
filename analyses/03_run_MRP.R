library(data.table)
library(here)
library(lme4)

if(interactive()){
  YEAR_RANGE <- "2017-2018"
  IND        <- "hbp"
  STAGE      <- "stage2"
  STATUS     <- "prevalence"
} else{
  args       <- commandArgs(trailingOnly = TRUE)
  ID         <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  taskmap    <- fread(here("data", "taskmap_MRP.csv"))
  YEAR_RANGE <- taskmap[ID, YR]
  IND        <- taskmap[ID, indicator]
  STAGE      <- taskmap[ID, stage]
  STATUS     <- taskmap[ID, status]
}

dt <- fread(here("data", "high_quality_dataset_meanBP_w_covariates_Sept24.csv"), 
            colClasses = list(character = "FIPS"))[year_range == YEAR_RANGE]

# Filter based on indicator
prev_var <- paste0("hbp_", STAGE)
if(IND == "hbp_diagnosis"){
  # index       <- dt[, get(prev_var)]
  # P(hypertension, aware)
  outcome_var <- "hbp_diagnosis"
} else if(IND == "hbp_bp"){
  # index <- dt[, get(prev_var) & hbp_diagnosis]
  outcome_var <- paste0("hbp_bp_", STAGE)
  # Redefine outcome variable to be P(hypertension, aware, uncontrolled) rather than P(uncontrolled | aware)
  # We model the joint probability directly since it is more straightforward to perform post-stratification
  dt[, (outcome_var) := get(outcome_var) & hbp_diagnosis]
} else{
  # index <- 1:nrow(dt)
  # P(hypertension)
  outcome_var <- prev_var
}

# dt <- dt[index]

# Change to factors with reference levels ---------------------------------
dt[, age_group := factor(age_group)]
dt[, ethnicity := factor(ethnicity, levels = c("white", "black", "hispanic", "asian", "other"))]
dt[, gender := factor(gender, levels = c("male", "female"))]
dt[, urban := factor(urban, levels = c(1, 0), labels = c("urban", "rural"))]
dt[, state_region := factor(state_region, levels = c("Northeast", "South", "North Central", "West"))]

# GLMER model fit -------------------------------------------------------------

# Nested random intercept (FIPS inside state)
# Random intercept for ethnicity, age group, and ethnicity:gender interaction
# Fixed effects: 
#   gender
#   urbanicity, unemployment rate, no HS degree rate, median household income, and region of state

formula <- as.formula(paste0(outcome_var, " ~ ", 
                    # "(1|state/FIPS) + (1|ethnicity) + (1|age_group) + (1|ethnicity:gender) + 
                    "(1|state/FIPS) + age_group + ethnicity*gender + 
                        urban + UE_rate + no_HS_rate + median_income + HI_coverage + state_region"))

fit.glmer <- glmer(
  formula,
  family = binomial(link = "logit"),
  data = dt,
  nAGQ = 1L,
  verbose = 1,
  control = glmerControl(optimizer = "nloptwrap")
)

filename <- paste0("glmer_", YEAR_RANGE, "_", STATUS, "_", STAGE, "_fixed-demo.rds")
saveRDS(fit.glmer, file = here("outputs-Sept24", filename))