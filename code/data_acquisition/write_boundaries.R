library(here)

source(here("R/get_boundaries.R"))

files <- c(here("data", "reference", "county_boundaries_2022.rds"),
           here("data", "reference", "state_boundaries_2022.rds"))

county <- get_boundaries("county", 2022)
state  <- get_boundaries("state", 2022)
   
saveRDS(county, file = files[1]  )
saveRDS(state, file = files[2] )