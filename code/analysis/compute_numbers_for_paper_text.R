# All numbers needed for the Results section

library(here)
library(data.table)

dt <- fread(here("data", "high_quality_dataset_meanBP_w_covariates_Sept24.csv"), colClasses = list(character = "FIPS"))

# Percentage in each year range
dt[, .N, year_range][order(year_range)][, N / sum(N)]
dt[, .N, gender][, N / sum(N)]
dt[, .N, age_group][, N / sum(N)]
dt[, .N, ethnicity][, N / sum(N)]


# Average age
dt[, .(mean(age), sd(age))]
dt[, .(mean(sbp), sd(sbp))]
dt[, .(mean(dbp), sd(dbp))]

PS <- fread(here("data", "poststratification_table.csv"))

# age
PS[, sum(n), age_group][, V1/sum(V1)]
PS[, sum(n), gender][, V1/sum(V1)]
PS[, .(sum(n)), ethnicity][, .(V1/sum(V1))]

# Highest county in Idaho, Oregon, and Washington Region
est <- fread(here("results/2023-2024_prevalence_stage2_county_fixed-demo.csv"))
est[state %in% c("OR", "WA", "ID")][order(mean)]
est[, gap := upper - lower]
