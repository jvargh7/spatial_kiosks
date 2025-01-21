library(here)
library(data.table)

# Cross-tabulation of all HBP estimates that need to be generated
YEARS    <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024", 
              "2017-2020", "2021-2024")
# OUTCOMES <- c("prevalence", "awareness", "controlled")
STAGES   <- c("stage1", "stage2")
LEVEL    <- c("national", "state", "county", 
              "age_group", "urban", "ethnicity", "gender", 
              "age-gender", "ethnicity-gender")

# taskmap_all <- CJ(YEARS, OUTCOMES, STAGES, LEVEL)
taskmap_all <- CJ(YEARS, STAGES, LEVEL)
taskmap_all[, type := ifelse(YEARS %in% c("2017-2020", "2021-2024"), "four", "two")]
taskmap_all <- taskmap_all[order(type, YEARS, STAGES)]
taskmap_all[, type := NULL]

fwrite(taskmap_all, here("data", "taskmap", "taskmap_final_estimates.csv"))