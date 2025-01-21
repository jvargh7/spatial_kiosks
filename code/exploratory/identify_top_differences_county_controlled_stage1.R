library(data.table)
library(ggplot2)
library(here)

compare_years <- function(outcome = "prevalence", stage = "stage2", level = "county", 
                          year_range_one = "2017-2020", year_range_two = "2021-2024"){

  old_path <- here("results/estimates", paste0(year_range_one, "_", outcome, "_", stage, "_", level, ".csv" ))
  new_path <- here("results/estimates", paste0(year_range_two, "_", outcome, "_", stage, "_", level, ".csv" ))
  
  if(level == "county"){
    class <- list(character = "FIPS")
  } else{
    class <- NULL
  }
  
  old <- fread(old_path, colClasses = class)[, -c("median", "lower", "upper")]
  setnames(old, "mean", "mean_old")
  new <- fread(new_path, colClasses = class)[, -c("median", "lower", "upper")]
  setnames(new, "mean", "mean_new")
  
  if(level != "national"){
    final <- merge(old, new)
  } else{
    final <- cbind(old, new)
  }
  
  return(final)
}

final.ac <- compare_years(outcome = "awareness-conditional", stage = "stage2", level = "county")
final.am <- compare_years(outcome = "awareness-marginal", stage = "stage2", level = "county")
final    <- compare_years(outcome = "awareness-conditional", stage = "stage2", level = "national")
final    <- compare_years(outcome = "prevalence", stage = "stage1", level = "county")
final    <- compare_years(outcome = "prevalence", stage = "stage2", level = "national")
final    <- compare_years(outcome = "awareness-marginal", stage = "stage2", level = "national")

final    <- compare_years(outcome = "prevalence", stage = "stage2", level = "national")
final    <- compare_years(outcome = "awareness-conditional", stage = "stage2", level = "national")
final    <- compare_years(outcome = "controlled", stage = "stage2", level = "national")

final    <- compare_years(outcome = "prevalence", stage = "stage2", level = "national")
final    <- compare_years(outcome = "awareness-conditional", stage = "stage2", level = "national")
final    <- compare_years(outcome = "controlled", stage = "stage2", level = "national")

ggplot(final.am, aes(mean_old, mean_new)) + 
  geom_point() + 
  geom_abline(aes(slope=1, intercept=0))