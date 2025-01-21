library(dplyr)
library(here)
library(rvest)

get_brfss_variable_layout <- function(year = 2021){
  # Construct the URL for each year
  url <- paste0("https://www.cdc.gov/brfss/annual_data/", year, "/llcp_varlayout_", substr(year,3,4), "_onecolumn.html")
  
  # Read the HTML content from the page
  webpage <- read_html(url)
  
  # Extract the table from the page
  brfss_table <- webpage %>%
    html_node("table") %>%
    html_table()
  
  # Display the first few rows of the table to verify (optional)
  print(paste("Year:", year))
  print(head(brfss_table))
  
  return(brfss_table)
}

