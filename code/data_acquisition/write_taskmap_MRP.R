library(data.table)
library(here)

# For each 2-yr range, we want to run models for 
# 1) HBP prevalence
# 2) HBP awareness (marginal)
# 3) HBP control (marginal)

taskmap <- CJ(YR = c("2017-2018", "2019-2020", "2021-2022", "2023-2024"),
              indicator = c("hbp", "hbp_diagnosis", "hbp_bp"),
              stage = c("stage1", "stage2"))

taskmap[, status := ifelse(indicator == "hbp", "prevalence", 
                           ifelse(indicator == "hbp_diagnosis", "awareness", "uncontrolled"))]

fwrite(taskmap, here("data", "taskmap", "taskmap_MRP.csv"))
