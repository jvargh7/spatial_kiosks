library(arrow)
library(data.table)
library(dplyr)
library(here)

# Code --------------------------------------------------------------------
pursuant   <- open_dataset(here("data", "kiosk-data-parquet-cleaned-Sept24"),
                           format = "parquet")
# > nrow(pursuant)
# [1] 79352233

# Filter down to high quality data (has HBP diagnosis)
pursuant_hbp_dt <- pursuant |>
  filter(hbp_diagnosis %in% c(0, 1)) |>
  collect() |>
  data.table()
# 
fwrite(pursuant_hbp_dt, here("data/pursuant_hbp_dt_Sept24.csv") )

pursuant_hbp_dt <- fread(here("data/pursuant_hbp_dt_Sept24.csv"), colClasses = list(character = "FIPS") )

# nrow(pursuant_hbp_dt)
# [1] 1280717

# Compute hypertension indicators 
pursuant_hbp_dt[, hbp_bp_stage1 := bp_systolic >= 130 | bp_diastolic >= 80]
pursuant_hbp_dt[, hbp_bp_stage2 := bp_systolic >= 140 | bp_diastolic >= 90]

# Account ID --------------------------------------------------------------

# Those with account IDs
acct <- pursuant_hbp_dt[account_id_mask != ""] |>
  group_by(account_id_mask, pseudo_member_id,
           FIPS, county, state, year, year_range,
           # FIPS, county, state, year_range,
           age, age_group, gender, ethnicity, urban) |>
  summarise(sbp = mean(bp_systolic),
            dbp = mean(bp_diastolic),
            hbp_diagnosis = ifelse(mean(hbp_diagnosis) > 0.5, 1, 0)
          ) |>
  # summarise(hbp_agg = ifelse(mean(hbp) > 0.5, 1, 0),
  #           hbp_bp = ifelse(mean(hbp_bp) > 0.5, 1, 0),
  #           hbp_diagnosis = ifelse(mean(hbp_diagnosis) > 0.5, 1, 0)
  #         ) |>
  # mutate(pseudo_member_id = "") |>
  ungroup() |>
  data.table()

# It's likely pseudo ID's are more random people who record multiple measurements in one sitting. 
# Identify how many pseudo ID's are taken in quick succession of each other

# Those with no account IDs, we rely on pseudo ID OR we don't 
pseudo <- pursuant_hbp_dt[account_id_mask == ""] |>
  group_by(pseudo_member_id,
           FIPS, county, state, year, year_range,
           # FIPS, county, state, year_range,
           age, age_group, gender, ethnicity, urban) |>
  summarise(sbp = mean(bp_systolic),
            dbp = mean(bp_diastolic),
            hbp_diagnosis = ifelse(mean(hbp_diagnosis) > 0.5, 1, 0)
          ) |>
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
  mutate(hbp_bp_stage1 = ( sbp >= 130 | dbp >= 80) ) |>
  mutate(hbp_bp_stage2 = ( sbp >= 140 | dbp >= 90) ) |>
  mutate(hbp_stage1 = ( hbp_diagnosis == 1 | hbp_bp_stage1 == 1) ) |>
  mutate(hbp_stage2 = ( hbp_diagnosis == 1 | hbp_bp_stage2 == 1) ) |>
  filter(state != "PR") |>
  data.table() 
# no mean: 1,213,188
# no mean: 1,213,188

fwrite(pursuant_hbp, here("data/high_quality_dataset_meanBP_Sept24.csv"))
