library(arrow)
library(data.table)
library(dplyr)
library(here)

if(interactive()){
  TASK_ID    <- 1
  STATE      <- "RI"
  YEAR_RANGE <- "2023-2024"
} else{
  # Get the array index from SLURM_ARRAY_TASK_ID
  map        <- fread(here("data", "taskmap.csv"))
  TASK_ID    <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  STATE      <- map[TASK_ID, state]
  YEAR_RANGE <- map[TASK_ID, year_range]
}

pursuant   <- open_dataset(here("data", "kiosk-data-parquet-cleaned-4"),
                           format = "parquet") 

# pseudo_member_id: DOB, Gender, ethnicity, location (No Time). 
# DOB narrows it down significantly but is not perfect (especially in a 2-year range). 
# What if we treat new members as pseudo_member_IDs within 1 year? 
# But pseudo_member_id can be missing: 
# if any of the above are missing (DOB, Gender, ethnicity)

dt <- pursuant |>
  filter(state == STATE, year_range == YEAR_RANGE) |>
  collect() |>
  data.table()

keyvar <- c("session_id_mask", "pseudo_member_id", 
            "age", "age_group", "gender", "ethnicity",
            "year", "year_range", "FIPS", "county", "state", "urban")

setkeyv(dt, keyvar)

nrow_all <- nrow(dt)

# Group by Session ID -----------------------------------------------------
dt_agg <- dt[, .(bp_systolic = median(bp_systolic),
                 bp_diastolic = median(bp_diastolic),
                 hbp_diagnosis = max(hbp_diagnosis, na.rm = TRUE)), keyvar]

nrow_session <- nrow(dt_agg)

# Group by Pseudo Member ------------------------------------------------
dt_id <- dt_agg[,.(bp_systolic = median(bp_systolic),
                   bp_diastolic = median(bp_diastolic),
                   hbp_diagnosis = max(hbp_diagnosis, na.rm = TRUE)), 
                 .(pseudo_member_id, age_group, gender, ethnicity, year_range, 
                   FIPS, county, state, urban)]

nrow_id <- nrow(dt_id)


# Number of rows ----------------------------------------------------------
dt_nrow <- data.table(state = STATE, year_range = YEAR_RANGE, nrow_all = nrow_all, nrow_session = nrow_session, 
           nrow_id = nrow_id)

# Final -------------------------------------------------------------------
# Compute hypertension indicator
dt_id[, hbp := hbp_diagnosis == 1 | bp_systolic >= 140 | bp_diastolic >= 90]

ID   <- sprintf("%03d", TASK_ID)
file <- paste0(ID, "_", STATE, "_", YEAR_RANGE, ".csv")
file_nrow <- paste0(ID, "_", STATE, "_", YEAR_RANGE, "_nrow.csv")
fwrite(dt_id, here("data", "temp", "split", file))
fwrite(dt_nrow, here("data", "temp", "nrow", file_nrow))