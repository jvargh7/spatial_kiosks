library(here)
library(data.table)

YEARS    <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024")
OUTCOMES <- c("prevalence", "awareness", "uncontrolled")
STAGES   <- c("stage1", "stage2")
LEVEL    <- c("national", "state", "county", "age_group", "urban", "ethnicity", "gender", 
              "age-gender", "ethnicity-gender")

taskmap_all <- CJ(YEARS, OUTCOMES, STAGES, LEVEL)

fwrite(taskmap_all, here("data", "taskmap_all_final_estimates.csv"))
