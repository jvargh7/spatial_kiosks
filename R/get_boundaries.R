library(dplyr)
library(here)
library(sf)
library(tigris)

#' @param level administrative level for the boundaries
#' @param year Year of boundary data (affects Connecticut the most, counties to "planning regions")
get_boundaries <- function(level = "county", year = 2022, ...){
  if(level == "county"){
    boundaries <- tigris::counties(class="sf",cb=TRUE, year = year, ...)  
  } else if(level == "state"){
    boundaries <- tigris::states(class = "sf", cb = TRUE, year = year, ...) 
  } else{
    stop("Invalid argument for `level`. `county` and `state` allowed only.")
  }
      
  boundaries <- boundaries |>
    tigris::shift_geometry()  |> 
    # Remove Puerto Rico
    dplyr::filter(STATEFP < 60) 
  
  return(boundaries)
}

# Write files if not in data/reference ------------------------------------
files <- c(here("data", "reference", "county_boundaries_2022.rds"),
           here("data", "reference", "state_boundaries_2022.rds"))
if(all(!file.exists(files))){
  county <- get_boundaries("county", 2022)
  state  <- get_boundaries("state", 2022)
  
  saveRDS(county, file = files[1]  )
  saveRDS(state, file = files[2] )
}