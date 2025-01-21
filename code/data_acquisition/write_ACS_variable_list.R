library(here)
library(tidycensus)

vars       <- load_variables(2022, "acs5", cache = TRUE) 
fwrite(vars, here("data", "reference", "ACS2022_variables.csv") )