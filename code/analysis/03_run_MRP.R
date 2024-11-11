library(data.table)
library(here)
library(lme4)
library(splines)

if(interactive()){
  YEAR_RANGE <- "2017-2018"
  IND        <- "hbp_bp"
  STAGE      <- "stage2"
  STATUS     <- "uncontrolled"
} else{
  args       <- commandArgs(trailingOnly = TRUE)
  ID         <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  taskmap    <- fread(here("data", "taskmap", "taskmap_MRP.csv"))
  YEAR_RANGE <- taskmap[ID, YR]
  IND        <- taskmap[ID, indicator]
  STAGE      <- taskmap[ID, stage]
  STATUS     <- taskmap[ID, status]
}

dt <- fread(here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv"), 
            colClasses = list(character = "FIPS"))[year_range == YEAR_RANGE]

# Filter based on indicator
prev_var <- paste0("hbp_", STAGE)

# P(D|H)
if(IND == "hbp_diagnosis"){
  outcome_var <- "hbp_diagnosis"
  dt <- dt[get(paste0("hbp_", STAGE))]
# P(C|D, H)
} else if(IND == "hbp_bp"){
  outcome_var <- paste0("!hbp_bp_", STAGE)
  # dt[, (outcome_var) := get(outcome_var) & hbp_diagnosis]
  dt <- dt[ get(paste0("hbp_", STAGE)) & hbp_diagnosis]
# P(H)
} else{
  outcome_var <- prev_var
}

# Change to factors with reference levels ---------------------------------
dt[, age_group := factor(age_group)]
dt[, ethnicity := factor(ethnicity, levels = c("white", "black", "hispanic", "asian", "other"))]
dt[, gender := factor(gender, levels = c("male", "female"))]
dt[, urban := factor(urban, levels = c(1, 0), labels = c("urban", "rural"))]
dt[, state_region := factor(state_region, levels = c("Northeast", "South", "North Central", "West"))]

# GLMER model fit -------------------------------------------------------------

# Nested random intercept (FIPS inside state)
# Random intercept:
#   State
#   County
# Fixed effects: 
#   gender
#   ethnicity
#   gender
#   age group
#   ethnicity:gender interaction
#   urbanicity, unemployment rate, no HS degree rate, median household income, and region of state

formula <- as.formula(paste0(outcome_var, " ~ ", 
                    # "(1|state/FIPS) + (1|ethnicity) + (1|age_group) + (1|ethnicity:gender) + 
                    "(1|state/FIPS) + age_group + ethnicity*gender + 
                        urban + ns(UE_rate, df = 4) + ns(no_HS_rate, df = 4) + ns(median_income, df = 4) + ns(HI_coverage, df = 4) + state_region"))

fit.glmer <- glmer(
  formula,
  family = binomial(link = "logit"),
  data = dt,
  # nAGQ = 1L,
  verbose = 1,
  control = glmerControl(optimizer = "nloptwrap")
)

filename <- paste0("glmer_", YEAR_RANGE, "_", STATUS, "_", STAGE, ".rds")
saveRDS(fit.glmer, file = here("results", "models", filename))