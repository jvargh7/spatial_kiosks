# Change all CT (Connecticut) FIPS to the 9 Planning Regions in the map to count --------
library(here)
library(data.table)
library(sf)
library(tidycensus)

# Change all CT FIPS codes in the Pursuant address map from old counties to new Planning Regions based on 2022 ACS
map  <- fread(here("data", "raw", "pursuant_public_kiosk_address_w_county.csv"), 
                colClasses = c(FIPS = 'character', zipcode = 'character'))

map[, zipcode := ifelse(nchar(zipcode) == 3, paste0("00", zipcode),
                        ifelse(nchar(zipcode) == 4, paste0("0", zipcode), zipcode))]
map[, state := ifelse(state == "Ak", "AK", state)]

map.CT <- map[state == "CT"]

ct <- tidycensus::get_acs(state = "CT", geography = "county",
                          variables = "B19013_001", geometry = TRUE, year = 2022)
# convert the points to same CRS
pts <- map.CT |>
  st_as_sf(coords = c("longitude", "latitude"),
         crs = st_crs(ct))

pts$FIPS <- ct$GEOID[as.numeric(st_within(pts, ct))] # this is fast for 1e6 points
pts$county <- ct$NAME[as.numeric(st_within(pts, ct))]

coords <- sf::st_coordinates(pts)
colnames(coords) <- c("longitude", "latitude")

pts <- data.table(pts)
pts[, geometry := NULL]
pts <- cbind(pts, coords)

map.new <- rbind(map[state != "CT"], pts)

# Merge in the RUCC urban/rural status
rucc <- fread(here("data", "reference", "RUCC2023.csv"), 
                colClasses = c(FIPS = "character"))[, .(FIPS, urban)]
map.all <- merge.data.table(map.new, rucc, by = "FIPS")
map.all[, `:=`(latitude = NULL, longitude = NULL, address_id = NULL)]
map.all <- unique(map.all)

fwrite(map.all, here("data", "reference", "pursuant_public_kiosk_address_w_county_CT_adj.csv"))