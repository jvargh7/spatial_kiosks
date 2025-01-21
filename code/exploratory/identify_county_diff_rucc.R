library(arrow)
library(data.table)
library(dplyr)
library(here)

# Investigate FIPS discrepancy between the Pursuant address map and RUCC file
# Answer: All discrepancies are in Connecticut. They changed from counties to "planning regions". 

# Details on the change: https://www.federalregister.gov/documents/2022/06/06/2022-12063/change-to-county-equivalents-in-the-state-of-connecticut
# RUCC: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/

map  <- fread(here("data", "raw", "pursuant_public_kiosk_address_w_county.csv"), 
                colClasses = c(FIPS = 'character'))
rucc <- fread(here("data", "reference", "RUCC2023.csv"), colClasses = c(FIPS = "character"))
# Check all counties in the dataset are in the RUCC -----------------------
dt   <- open_dataset(here("data", "raw", "emory-limited-data-set-export-Sept24"),format = "parquet") 
FIPS <- dt |>
          mutate(bp_systolic = as.numeric(bp_systolic)) |>
          filter(between(bp_systolic, 60, 300)) |>
          mutate(bp_diastolic = as.numeric(bp_diastolic)) |>
          filter(between(bp_systolic, 30, 300)) |>
          left_join(map, by = c("street1", "city", "state")) |>
          count(FIPS) |>
           collect() |>
           data.table()

unique.fips <- unique(FIPS$FIPS)
missing_rucc_fips <- unique.fips[!(unique.fips %in% rucc$FIPS)]
# [1] "09001" "09005" "09003" "09009" "09011" "09015" NA      "09007

# The only FIPS missing in the RUCC are old CT county codes and NA / missing. 
# Don't need to worry about NA for now. But how to map the CT codes?

# TLDR --------------------------------------------------------------------

# OLD FIPS CODES used in the Pursuant mapping
FIPS[substr(FIPS, 1, 2) == "09", unique(FIPS)]
# "09001" "09005" "09003" "09009" "09011" "09015" "09007"
# NEW FIPS CODES (planning regions) actually used in CT and the RUCC
rucc[State == "CT", unique(FIPS)]
# [1] "09110" "09120" "09130" "09140" "09150" "09160" "09170" "09180" "09190"