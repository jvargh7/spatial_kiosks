library(arrow)
library(data.table)
library(dplyr)
library(here)

# Code --------------------------------------------------------------------
pursuant   <- open_dataset(here("data", 
                                "processed", 
                                "kiosk-data-parquet-cleaned-Sept24"),
                           format = "parquet")

# Filter down to high quality data (has HBP diagnosis)
pursuant_hbp_dt <- pursuant |>
  filter(hbp_diagnosis %in% c(0, 1)) |>
  collect() |>
  data.table()
 
fwrite(pursuant_hbp_dt, here("data/processed/high_quality_dt_before_deduplication_Sept24.csv") )

# pursuant_hbp_dt <- fread(here("data/processed/high_quality_dt_before_deduplication_Sept24.csv"), 
#                          colClasses = list(character = "FIPS") )

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

pursuant_hbp <- acct |>
  rbind(pseudo) |> 
  mutate(hbp_bp_stage1 = ( sbp >= 130 | dbp >= 80) ) |>
  mutate(hbp_bp_stage2 = ( sbp >= 140 | dbp >= 90) ) |>
  mutate(hbp_stage1 = ( hbp_diagnosis == 1 | hbp_bp_stage1 == 1) ) |>
  mutate(hbp_stage2 = ( hbp_diagnosis == 1 | hbp_bp_stage2 == 1) ) |>
  filter(state != "PR") |>
  data.table() 

# Merge with covariates (already standardised)
cov <- fread(here("data", "reference", "model_covariates.csv"), 
             colClasses = list(character = c("FIPS", "state_code")))

dt_cov <- pursuant_hbp |>
  # merge(cov[, .(FIPS, state_region, UE_rate, no_HS_rate, median_income, HI_coverage)], by = "FIPS", all.x = TRUE)
  merge(cov[, .(FIPS, state_region, UE_rate, no_HS_rate, median_income, HI_coverage)], by = "FIPS")

# Some problems with missing locations...should probably look into this!
fwrite(dt_cov[!is.na(median_income)], here("data",
                                           "processed",
                                           "high_quality_dt_after_deduplication_w_cov_Sept24.csv"))
