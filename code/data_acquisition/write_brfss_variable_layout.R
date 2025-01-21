library(here)

source(here("R/get_brfss_variable_layout.R"))

for(year in 2017:2023){
  tab <- get_brfss_variable_layout(year)
  # Save the table as a CSV file
  csv_filename <- here("data", "raw", "brfss", "variable_layouts", paste0(year, ".csv") )
  data.table::fwrite(tab, csv_filename)
}
