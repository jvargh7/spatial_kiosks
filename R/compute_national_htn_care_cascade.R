library(data.table)
library(dplyr)
library(here)
library(gt)

compute_national_htn_care_cascade <- function(){
  path <- here("results/estimates")
  
  filenames <- list.files(path, pattern = "national", full.names = FALSE)
  filepaths <- list.files(path, pattern = "national", full.names = TRUE)
  
  mean <- sapply(filepaths, function(x) fread(x)[, mean], USE.NAMES = FALSE) 
  lower <- sapply(filepaths, function(x) fread(x)[, lower], USE.NAMES = FALSE) 
  upper <- sapply(filepaths, function(x) fread(x)[, upper], USE.NAMES = FALSE) 
  
  mean <- ifelse(mean < 1, mean*100, mean)
  lower <- ifelse(lower < 1, lower*100, lower)
  upper <- ifelse(upper < 1, upper*100, upper)
  
  values <- strsplit(filenames, "_")
  
  dt <- do.call(rbind, values) |> data.table() |>
    setnames(c("V1", "V2", "V3"), c("year_range", "outcome", "stage"))
  dt[, V4 := NULL]
  
  dt[, mean := mean]
  dt[, lower := lower]
  dt[, upper := upper]
  
  # Filter for the desired year ranges
  filtered_data <- dt %>%
    filter(year_range %in% c("2017-2018", "2019-2020", "2021-2022", "2023-2024"))
  
  # Format the table with mean and confidence intervals
  formatted_table <- filtered_data %>%
    mutate(
      stage = case_when(
        stage == "stage1" ~ "Stage 1 Hypertension", # Rename Stage 1
        stage == "stage2" ~ "Stage 2 Hypertension", # Rename Stage 2
        outcome == "awareness-marginal" ~ "Awareness Marginal", # Make Awareness Marginal its own group
        TRUE ~ stage
      ),
      outcome = gsub("-", " ", tools::toTitleCase(outcome)), # Capitalize and replace hyphens with spaces
      mean_with_ci = sprintf("%.1f (%.1f, %.1f)", mean, lower, upper) # Combine mean and CI
    ) %>%
    select(year_range, outcome, stage, mean_with_ci) %>%
    arrange(factor(stage, levels = c("Awareness Marginal", "Stage 1 Hypertension", "Stage 2 Hypertension")),
            factor(outcome, levels = c("Prevalence", "Awareness Conditional", "Controlled"))) %>% # Arrange Awareness Marginal above other stages
    tidyr::pivot_wider(
      names_from = year_range,
      values_from = mean_with_ci
    ) |>
    #filter(outcome != "Awareness Marginal") |>
    mutate(outcome = ifelse(outcome == "Awareness Conditional", "Awareness", outcome))
  
  # Create the table using gt
  gt_table <- formatted_table %>%
    gt(groupname_col = "stage") %>% # Use stage as the grouping variable
    tab_header(
      title = "National Partial Hypertension Care Cascade",
      subtitle = "Estimated by MRP on Pursuant data"
    ) %>%
    cols_label(
      outcome = "Outcome",
      `2017-2018` = "2017-2018",
      `2019-2020` = "2019-2020",
      `2021-2022` = "2021-2022",
      `2023-2024` = "2023-2024"
    ) |>
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) |>
    tab_style(
      style = list(
        cell_text(indent = px(20)) # Add 20px indentation
      ),
      locations = cells_body(
        columns = outcome, # Indent the "Outcome" column
        rows = !is.na(stage) # Apply to rows under the grouped stages
      )
    )
  
  return(gt_table)
}

