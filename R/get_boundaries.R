library(dplyr)
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