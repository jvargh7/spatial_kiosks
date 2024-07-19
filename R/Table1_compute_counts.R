library(arrow)
library(dplyr)
library(rlang)

#' @param variable the variable(s) to summarize over
#' @param dt arrow dataset object
#' @param by_pseudo_id logical argument for whether to aggregate over pseudo ID
#' @param year_range optional year-range argument to filter observations
Table1_compute_counts <- function(variable, dt, by_pseudo_id = FALSE, year_range = NULL){
  data <- dt |>
      filter(year_range == year_range) 
  if(by_pseudo_id == FALSE){
    N <- nrow(data)  
    summary <- data |>
      count(!!! rlang::syms(variable) ) |>
      collect() |>
      mutate(percent = n / sum(n) * 100) 
  } else{
    summary <- data |>
      count(pseudo_member_id, !!! rlang::syms(variable)) |>
      count(!!! rlang::syms(variable)) |>
      collect() |>
      mutate(percent = n / sum(n) * 100) 
  }
  return(summary)
}

generate_Table1 <- function(dt){
  all_vars <- c("age_group", "gender", "ethnicity", "urban")
  
  Column2_all_sessions <- lapply(all_vars, Table1_compute_counts, dt = dt, by_pseudo_id = FALSE, year_range = NULL)
  Column3_all_pseudo_id <- lapply(all_vars, Table1_compute_counts, dt = dt, by_pseudo_id = TRUE, year_range = NULL)
  Column4_all_pseudo_id <- lapply(all_vars, Table1_compute_counts, dt = dt, by_pseudo_id = FALSE, year_range = "2021-2022")

  saveRDS(list(Column2 = Column2_all_sessions,
               Column3 = Column3_all_pseudo_id,
               Column4 = Column4_all_pseudo_id), here("tables", "Table1.rds") )
}