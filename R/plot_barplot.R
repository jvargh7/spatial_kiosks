library(here)
library(data.table)
library(dplyr)
  data_est <- here("data/high_quality_dataset_meanBP_w_covariates.csv") |>
    fread()

  
  data_est[, .N / nrow(data_est), age_group]  
  data_est[, .N / nrow(data_est), gender]  
  data_est[, .N / nrow(data_est), ethnicity]  
  
  data_est[, .N / nrow(data_est), urban]  
  
  year  <- "2021-2022"
  model <- "prevalence"
  stage <- "stage1"
  
  name <- paste(year, model, stage, sep = "_")
  types <- c("ethnicity", "ethnicity-gender")
  
  filenames <- paste(name, types, "fixed-demo.csv", sep = "_")
  
  files <- here("results", filenames)
  dt.list <- lapply(files, fread)
  dt.list[[1]][, gender := "All"]
  
  dt <- rbindlist(dt.list, use.names = TRUE) |>
    mutate(ethnicity = factor(ethnicity, levels = c("white", "black", "hispanic", "asian", "other"), 
                              labels = c("NH White", "NH Black", "Hispanic", "NH Asian", "NH Other")),
           gender = factor(gender, levels = c("All", "male", "female"), 
                           labels = c("All", "Men", "Women")))
  
  ethnicity_gender_barchart <- 
    ggplot(dt, aes(gender, mean, fill = ethnicity)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_point(position = position_dodge(0.9), size = 0.5) +
    geom_errorbar(width=0.25,
      aes(
          ymin = lower,
          ymax = upper), position = position_dodge(0.9)
    ) +
    ylab("Prevalence (%)") + 
    xlab(NULL) + 
    theme_classic(base_size = 18) + 
    geom_text(aes(label=round(mean,1)), position=position_dodge(width=0.9), vjust=-1) +
    # geom_text(aes(label=round(mean,1)), position=position_dodge(width=0.9)) +
    theme(legend.position = "top") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
    scale_fill_manual(name = "", 
                      values = c("NH White" = "#586891FF", 
                                 "NH Black" = "#8897A4FF", 
                                 "Hispanic" = "#B3A6A3FF",
                                 "NH Asian" = "#2B152CFF",
                                 "NH Other" = "#F1F3F2FF")) 
  ggsave(filename = "Figure_stage1_ethnicity_gender_barplot.png", width = 8, height = 4)
    #586891FF, #8897A4FF, #B3A6A3FF, #2B152CFF, #F1F3F2FF

# Age ---------------------------------------------------------------------

  
year  <- "2017-2018"
model <- "prevalence"
stage <- "stage1"

name <- paste(year, model, stage, sep = "_")
types <- c("national", "gender", "age-gender", "age_group")

filenames <- paste(name, types, "fixed-demo.csv", sep = "_")

files <- here("results", filenames)
dt.list <- lapply(files, fread)
dt.list[[1]][, age_group := ">18"][, gender := "All"]
dt.list[[2]][, age_group := ">18"]
dt.list[[4]][, gender := "All"]

dt <- rbindlist(dt.list, use.names = TRUE) |>
      mutate(age_group = factor(age_group, levels = c(">18", "18-19", "20-44", "45-64", "65plus"),
                                labels = c("18+", "18 to <20", "20 to <45", "45 to <65", "65+")),
             gender = factor(gender, levels = c("All", "male", "female"), 
                              labels = c("All", "Men", "Women")))
  agegroup_gender_barchart <- 
    ggplot(dt, aes(gender, mean, fill = age_group)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_point(position = position_dodge(0.9), size = 0.5) +
    geom_errorbar(width=0.25,
      aes(
          ymin = lower,
          ymax = upper), position = position_dodge(0.9)
    ) +
    ylab("Prevalence (%)") + 
    xlab(NULL) + 
    theme_classic(base_size = 18) + 
    geom_text(aes(label=round(mean,1)), position=position_dodge(width=0.9), vjust=-1) +
    # geom_text(aes(label=round(mean,1)), position=position_dodge(width=0.9)) +
    theme(legend.position = "top") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
    scale_fill_manual(name = "", 
                      values = c("18+" = "#3182bd",
                                 "18 to <20" =  "#63589FFF", 
                                 "20 to <45" =  "#D1AFE8FF", 
                                 "45 to <65" = "#F3E0F7FF", 
                                 "65+" = "grey")) 
  ggsave(filename = "Figure_stage1_age_gender_barplot.png", width = 8, height = 4)
  
   # "#F3E0F7FF", "#E4C7F1FF", "#D1AFE8FF", "#B998DDFF", "#9F82CEFF", "#826DBAFF",