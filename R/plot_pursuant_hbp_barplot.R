library(arrow)
library(dplyr)
library(ggplot2)

# Compute summaries -------------------------------------------------------
compute_summary <- function(dt, variable, YEAR_RANGE = NULL){
  if("age_group" %in% variable){
    summary <- dt |>
      filter(year_range %in% YEAR_RANGE) |>
      mutate(age_group = ifelse(age_group %in% c("18-19", "20-44"), "18-44", age_group)) |>
      group_by(!!! rlang::syms(variable) ) |> 
      summarise(hbp = mean(hbp)) |> 
      collect() 
  } else{
    summary <- dt |>
      filter(year_range %in% YEAR_RANGE) |>
      group_by(!!! rlang::syms(variable) ) |> 
      summarise(hbp = mean(hbp)) |> 
      collect() 
  }
  return(summary)
}

# Sex-ethnicity -----------------------------------------------------------

plot_pursuant_hbp_by_ethnicity_gender_barchart <- function(dt, YEAR_RANGE = NULL){
  NHHES <- tibble(ethnicity=rep(c("NH White", "NH Black", "Hispanic"), times = 3), 
                  gender=rep(c("All", "Men", "Women"), each = 3), 
                  hbp=c(.436, .571, .437, .502, .572, .501, .367, .567, .368), 
                  type = "National Health and Nutrition Examination Survey (2017-2018)")
  plot_df <- 
    compute_summary(dt, "ethnicity", YEAR_RANGE) |>
    mutate(gender = "All") |>
    rbind( 
      compute_summary(dt, c("ethnicity", "gender"), YEAR_RANGE)
    ) |>
    mutate(ethnicity = factor(ethnicity, levels = c("white", "black", "hispanic", "asian", "other"), 
                              labels = c("NH White", "NH Black", "Hispanic", "NH Asian", "NH Other")),
           gender = factor(gender, levels = c("All", "male", "female"), 
                            labels = c("All", "Men", "Women")),
           type = paste0("Pursuant observational kiosk data direct estimate (", YEAR_RANGE, ")")) |>
    arrange(gender, ethnicity) |>
    filter(ethnicity %in% c("NH White", "NH Black", "Hispanic")) |>
    rbind(NHHES) |>
    mutate(hbp = hbp*100)
  
  ethnicity_barchart <- 
  ggplot(plot_df, aes(gender, hbp, fill = ethnicity)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Prevalence (%)") + 
    xlab(NULL) + 
    theme_classic(base_size = 18) + 
    geom_text(aes(label=round(hbp,1)), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(legend.position = "top") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) + 
    scale_fill_manual(name = "", 
                      values = c("NH White" = "#8856a7", 
                                 "NH Black" = "#3182bd", 
                                 "Hispanic" = "#bcbddc")) +
    facet_wrap(~type, labeller = labeller(type = label_wrap_gen(width = 50)))
}

plot_pursuant_hbp_by_agegroup_gender_barchart <- function(dt, YEAR_RANGE = NULL){
  plot_df <- 
    # Age group only
    compute_summary(dt, variable = "age_group", YEAR_RANGE = "2017-2018") |>
      mutate(gender = "All") |>
    # Age group + gender
      rbind( 
        compute_summary(dt, c("age_group", "gender"), YEAR_RANGE = "2017-2018")
      ) |>
    # No variable, all
      rbind(
        compute_summary(dt, variable = NULL, YEAR_RANGE = "2017-2018") |>
          mutate(gender = "All", age_group = ">18")
      ) |>
    # Gender only
      rbind(
        compute_summary(dt, variable = "gender", YEAR_RANGE = "2017-2018") |>
          mutate(age_group = ">18")
      ) |> 
      mutate(age_group = factor(age_group, levels = c(">18", "18-44", "45-64", "65plus"),
                                labels = c("18 and over", "18 to <45", "45 to <65", "65 plus")),
             gender = factor(gender, levels = c("All", "male", "female"), 
                              labels = c("All", "Men", "Women")),
             hbp = hbp*100)
  
  
  agegroup_gender_barchart <- 
    ggplot(plot_df, aes(gender, hbp, fill = age_group)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Prevalence (%)") + 
    xlab(NULL) + 
    theme_classic(base_size = 18) + 
    geom_text(aes(label=round(hbp,1)), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(legend.position = "top") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) + 
    scale_fill_manual(name = "", 
                      values = c("18 and over" = "#8856a7", 
                                 "18 to <45" = "#3182bd", 
                                 "45 to <65" = "#bcbddc", 
                                 "65 plus" = "grey")) 
  
  return(agegroup_gender_barchart)
}

plot_pursuant_hbp_by_agegroup_urban_barchart <- function(dt, YEAR_RANGE = NULL){
  plot_df <- 
    # Age group only
    compute_summary(dt, variable = "age_group", YEAR_RANGE = YEAR_RANGE) |>
      mutate(urban = -1) |>
    # Age group + urban
      rbind( 
        compute_summary(dt, c("age_group", "urban"), YEAR_RANGE = YEAR_RANGE)
      ) |>
    # No variable, all
      rbind(
        compute_summary(dt, variable = NULL, YEAR_RANGE = YEAR_RANGE) |>
          mutate(urban = -1, age_group = ">18")
      ) |>
    # Urban only
      rbind(
        compute_summary(dt, variable = "urban", YEAR_RANGE = YEAR_RANGE) |>
          mutate(age_group = ">18")
      ) |> 
      mutate(age_group = factor(age_group, levels = c(">18", "18-44", "45-64", "65plus"),
                                labels = c("18 and over", "18 to <45", "45 to <65", "65 plus")),
             urban = factor(urban, levels = c(-1, 0, 1), 
                              labels = c("All", "Rural", "Urban")),
             hbp = hbp*100)
  
  
  agegroup_urban_barchart <- 
    ggplot(plot_df, aes(urban, hbp, fill = age_group)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Prevalence (%)") + 
    xlab(NULL) + 
    theme_classic(base_size = 18) + 
    geom_text(aes(label=round(hbp,1)), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(legend.position = "top") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) + 
    scale_fill_manual(name = "", 
                      values = c("18 and over" = "#8856a7", 
                                 "18 to <45" = "#3182bd", 
                                 "45 to <65" = "#bcbddc", 
                                 "65 plus" = "grey")) 
  
  return(agegroup_urban_barchart)
  
}
