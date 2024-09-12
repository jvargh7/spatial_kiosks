library(data.table)
library(here)
library(readxl)

# Create urban/rural status for each county in the RUCC
rucc <- data.table( read_excel(here("data", "Ruralurbancontinuumcodes2023.xlsx")) )
rucc[, urban := ifelse(RUCC_2023 %in% 1:3, 1, 0)]

fwrite(rucc, here("data", "reference", "RUCC2023.csv"))
