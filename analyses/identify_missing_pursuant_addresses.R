library(arrow)
library(dplyr)
library(here)
library(data.table)

# Identify which addresses in the Pursuant dataset are not in the Pursuant address map--------
dt   <- open_dataset(here("data", "emory-limited-data-set-export-v2"),format = "parquet") 
map  <- fread(here("data", "reference", "pursuant_public_kiosk_address_w_county_CT_adj.csv"), 
              colClasses = c(FIPS = 'character'))#[, .(street1, city, state, FIPS, county, urban)]
address <- dt |> count(street1, city, state) |> collect()

# All addresses in the dataset which are not in the map
address$street1[!(address$street1 %in% map$street1)]
# [1] " 6225 Colony St"        "1123 US-79 "            "9820 Callabridge Court"
# [4] "3139 S HARVARD AVE "    "3101 W Kimberly Road"   " 1024 S State Rd 19"   
# [7] "602 Shelia St"          "1230 West 29th St. "    " 41301 US-280"         
# [10] "1600 E Tipton Street"  

# Trim whitespace
dt <- dt |> 
  mutate(street1 = gsub("^\\s+|\\s+$", "", street1))

address <- dt |> count(street1, city, state) |> collect()

# All addresses in the dataset which are not in the map, AFTER trimming whitespace
missing <- address$street1[!(address$street1 %in% map$street1)]
# [1] "602 Shelia St"          "3101 W Kimberly Road"   "9820 Callabridge Court"
# [4] "1600 E Tipton Street"  

address <- data.table(address)
address[grepl( "602 S", street1)]
address[grepl( "3101 W K", street1)]
address[grepl( "9820 C", street1)]
address[grepl( "1600 E T", street1)]

dt <- dt|>
  mutate(street1 = ifelse(street1 == "602 Shelia St", "602 SHELIA STREET", street1)) |>
  mutate(street1 = ifelse(street1 == "3101 W Kimberly Road", "3101 W Kimberly Rd", street1)) |>
  mutate(street1 = ifelse(street1 == "9820 Callabridge Court", "9820 Callabridge Ct", street1)) |>
  mutate(street1 = ifelse(street1 == "1600 E Tipton Street", "1600 E Tipton St", street1))

address <- dt |> count(street1, city, state) |> collect()

# All addresses in the dataset which are not in the map, AFTER trimming whitespace
missing <- address$street1[!(address$street1 %in% map$street1)]
