library(arrow)
library(data.table)
library(dplyr)
library(here)

# Code --------------------------------------------------------------------
pursuant   <- open_dataset(here("data", "kiosk-data-parquet-cleaned-5"),
                           format = "parquet") 
# > nrow(pursuant)
# [1] 79352233

# Filter down to high quality data (has HBP diagnosis)
pursuant_hbp_dt <- pursuant |> 
  filter(hbp_diagnosis %in% c(0, 1)) |>
  collect() |>
  data.table()

# nrow(pursuant_hbp_dt)
# [1] 1280717

# Compute hypertension indicators 
pursuant_hbp_dt[, hbp_bp := bp_systolic >= 140 | bp_diastolic >= 90]
pursuant_hbp_dt[, hbp := hbp_diagnosis == 1 | bp_systolic >= 140 | bp_diastolic >= 90]
# pursuant_hbp_dt[, hbp_stage1 := hbp_diagnosis == 1 | bp_systolic >= 130 | bp_diastolic >= 80]

# Account ID --------------------------------------------------------------
# Compare deduplication stratgies
# 1. aggregate over pseudo ID (blood pressure)
# 2. aggregate over pseudo ID (hbp_diagnosis)
# 3. aggregate over account ID, then pseudo ID

# Limitations
#
# Multiple people can belong to the same pseudo ID
# An individual people can be associated with multiple pseudo ID (use kiosk across the country)
# We have no way to track. We use pseudo ID to approximate account ID. 
# We summarize an individual-year by the "majority" of blood pressure diagnoses. 

# How to summarize over location? 

# If account ID spans across locations, let it count for each location. But we only take one per year
# Need to quantify how many observations out of total are due to multiple measurements. 

pursuant_hbp_dt[account_id_mask != "", .N,account_id_mask][,mean(N>1)]

# > pursuant_hbp_dt[account_id_mask != "", .N,account_id_mask][,mean(N>1)]
# [1] 0.07861626

# 7.9% of account holders recorded multiple measurements. 
# Average / median number of measurements? 
# 2 measurements, or 2.

# > pursuant_hbp_dt[account_id_mask != "", .N,account_id_mask][N>1][,median(N)]
# [1] 2
# > pursuant_hbp_dt[account_id_mask != "", .N,account_id_mask][N>1][,mean(N)]
# [1] 2.732622

# 2.5%   25%   50%   75% 97.5% 
# 2     2     2     3     7 

# Those with account IDs
acct <- pursuant_hbp_dt[account_id_mask != ""] |>
  group_by(account_id_mask, 
           FIPS, county, state, year, year_range,
           age, age_group, gender, ethnicity, urban) |>
  summarise(hbp_agg = ifelse(mean(hbp) > 0.5, 1, 0),
            hbp_bp = mean(hbp_bp),
            hbp_diagnosis = mean(hbp_diagnosis),
          ) |>
  mutate(pseudo_member_id = "") |>
  ungroup() |>
  data.table()

# It's likely pseudo ID's are more random people who record multiple measurements in one sitting. 
# Identify how many pseudo ID's are taken in quick succession of each other

# Those with no account IDs, we rely on pseudo ID OR we don't 
pseudo <- pursuant_hbp_dt[account_id_mask == ""] |>
  group_by(pseudo_member_id,
           FIPS, county, state, year, year_range,
           age, age_group, gender, ethnicity, urban) |>
  summarise(hbp_agg = ifelse(mean(hbp) > 0.5, 1, 0),
            hbp_bp = mean(hbp_bp),
            hbp_diagnosis = mean(hbp_diagnosis)) |>
  mutate(account_id_mask = "") |>
  ungroup() |>
  data.table()

# There is reason to believe frequent flyers are only those with account IDs.
# Any other impact is minimal. 
# Another opportunity for sensitivity analysis.

# pseudo2 <- pursuant_hbp_dt[account_id_mask == ""] |>
#   mutate(hbp_agg = hbp) |>
#   select(account_id_mask, pseudo_member_id, 
#          FIPS, county, state, year, year_range, 
#          age, age_group, gender, ethnicity, urban, hbp_agg, hbp_bp, hbp_diagnosis)

# pursuant_hbp <- acct |>
#   rbind(pseudo) |>
#   data.table()

pursuant_hbp <- acct |>
  rbind(pseudo) |>
  data.table()
fwrite(pursuant_hbp, "data/high_quality_dataset.csv")
