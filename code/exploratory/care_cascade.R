library(data.table)
library(dplyr)
dt <- fread("data/high_quality_dataset_meanBP.csv", colClasses = list(character = "FIPS"))

# Care cascade ------------------------------------------------------------

# Calculate care cascade by any variable
# age-standardize to the total analytic sample age distribution

calc_table <- function(dt, hbp_var, type, strata=NULL, age_standardize = FALSE){
  if(age_standardize){
    standard.pop <- dt |>
      count(age_group) |>
      mutate(weight = n / sum(n), n = NULL)
    total.prev <- dt |>
        group_by(!!sym(strata), age_group) |>
        summarise(hbp_prev = mean(!!sym(hbp_var)), 
                  num = sum(!!sym(hbp_var) == 1),
                  denom = n(), .groups = "drop") |>
        inner_join(standard.pop, by = "age_group") |>
        group_by(!!sym(strata)) |>
        summarise(hbp_prev = sum(weight * hbp_prev ),
                  num = sum(weight * num),
                  denom = sum(weight * denom)) |>
        mutate(sd = sqrt(hbp_prev * (1- hbp_prev) / num)) |>
        mutate(lower = hbp_prev - 1.96*sd, upper = hbp_prev + 1.96*sd, type = type) 
  }else{
    if(is.null(strata)){
      total.prev <- dt |>
          summarise(hbp_prev = mean(!!sym(hbp_var)), 
                    num = sum(!!sym(hbp_var) == 1),
                    denom = n(), .groups = "drop") |>
          mutate(sd = sqrt(hbp_prev * (1- hbp_prev) / num)) |>
          mutate(lower = hbp_prev - 1.96*sd, upper = hbp_prev + 1.96*sd, type = type) 
    } else{
      total.prev <- dt |>
          group_by(!!sym(strata)) |>
          summarise(hbp_prev = mean(!!sym(hbp_var)), 
                    num = sum(!!sym(hbp_var) == 1),
                    denom = n(), .groups = "drop") |>
          mutate(sd = sqrt(hbp_prev * (1- hbp_prev) / num)) |>
          mutate(lower = hbp_prev - 1.96*sd, upper = hbp_prev + 1.96*sd, type = type) 
    }
  }
  return(total.prev)
}

care_cascade <- function(dt, strata = NULL, age_standardize=FALSE){
  # Overall
  if(is.null(strata)){
    total.prev <- dt |>
      calc_table("hbp", "Hypertension") |> 
      rbind(
        dt |> 
          filter(hbp == 1) |>
          calc_table("hbp_diagnosis", type = "Aware")
      ) |> 
      rbind(
        dt |> 
          filter(hbp_diagnosis == 1) |>
          calc_table("hbp_bp", type = "Uncontrolled")
      ) |>
      select(type, hbp_prev, num, denom, sd, lower, upper)
  } else{
    total.prev <- dt |>
      calc_table("hbp", "Hypertension", strata = strata, age_standardize) |> 
      rbind(
        dt |> 
          filter(hbp == 1) |>
          calc_table("hbp_diagnosis", type = "Aware", strata = strata, age_standardize)
      ) |> 
      rbind(
        dt |> 
          filter(hbp_diagnosis == 1) |>
          calc_table("hbp_bp", type = "Uncontrolled", strata = strata, age_standardize)
      ) |>
      select(type, !!sym(strata), hbp_prev, num, denom, sd, lower, upper)
    
    
  }
  return(total.prev)
}

total  <- care_cascade(dt)
gender <- care_cascade(dt, "gender", TRUE)
ethn   <- care_cascade(dt, "ethnicity", TRUE)
age    <- care_cascade(dt, "age_group", FALSE)
urban  <- care_cascade(dt, "urban", TRUE)

saveRDS(list(total, gender, ethn, age, urban), file = "data/care_cascade_meanBP.rds")
