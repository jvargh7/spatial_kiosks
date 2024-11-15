library(data.table)
library(here)

# For each 2-yr range, we want to run models for 
# 1a) HBP prevalence
# 1b) HBP awareness (marginal)
# 2)  HBP awareness given prevalence 
# 3)  HBP control given awareness and prevalence

taskmap <- CJ(YR = c("2017-2018", "2019-2020", "2021-2022", "2023-2024",
                     "2017-2020", "2021-2024"),
              indicator = c("hbp", "hbp_diagnosis", "hbp_bp"),
              stage = c("stage1", "stage2"))

taskmap[, status := ifelse(indicator == "hbp", "prevalence", 
                           ifelse(indicator == "hbp_diagnosis", "awareness", "controlled"))]

taskmap[, distribution := ifelse(status %in% c("awareness", "controlled"), "conditional", "marginal")]

# Replicate table for the marginal awareness model (to compare to BRFSS)
other <- taskmap[status == "awareness"]
other[, distribution := "marginal"]

taskmap <- rbind(taskmap, other)[order(YR, status)]
fwrite(taskmap, here("data", "taskmap", "taskmap_MRP.csv"))
