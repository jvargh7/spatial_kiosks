# All numbers needed for the Abstract and Results.

library(here)
library(data.table)

# Results -----------------------------------------------------------------
dt <- fread(here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv"), 
            colClasses = list(character = "FIPS"))

# Percentage in each year range
dt[, .(percent=.N/nrow(dt)), year_range][order(year_range)]

# Mean (SD) for age, SBP, and DBP
dt[, .(mean = mean(age), SD = sd(age))]
dt[, .(mean = mean(sbp), SD = sd(sbp))]
dt[, .(mean = mean(dbp), SD = sd(dbp))]

