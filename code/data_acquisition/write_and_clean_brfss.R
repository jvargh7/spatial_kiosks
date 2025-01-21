library(data.table)
library(dplyr)
library(here)
library(vroom)

columns <- fread(here("data/raw/brfss/variable_layouts/2021.csv"))

columns$File_Width = sapply(1:nrow(columns), function(y) ifelse(y < nrow(columns), 
                                                                columns$`Starting Column`[y + 1] - columns$`Starting Column`[y], 1))

columns = columns[columns$File_Width > 0,]

# Read the fixed-width file using vroom_fwf
brfss_data <- vroom_fwf(
  file = here("data/raw/brfss/ascii/LLCP2021.ASC "),  # Path to your .ASC file
  col_positions = fwf_widths(columns$File_Width, columns$`Variable Name`)  # Use the column widths and names defined above
  # col_types = vroom::cols(.default = "c")  # Specify column types (optional, here all set to character)
) |>
  select("_STATE", "_AGEG5YR", "_SEX", "_RACE", "_URBSTAT", "_RFHYPE6") |> 
  mutate(sex = case_when( 
                         `_SEX` == 1 ~ "Male",
                         `_SEX` == 2 ~ "Female")) |>
  mutate(urban = case_when(
    `_URBSTAT` == 1 ~ "urban",
    `_URBSTAT` == 2 ~ "rural"
  )) |>
  mutate(hbp = case_when(
    `_RFHYPE6` == 1 ~ FALSE,
    `_RFHYPE6` == 2 ~ TRUE
  ))  |> 
  mutate(race = case_when(
    `_RACE` == 1 ~ "NH white",
    `_RACE` == 2 ~ "NH black",
    `_RACE` %in% c(3,5,6,7,9) ~ "NH other",
    `_RACE` == 4 ~ "NH asian",
    `_RACE` == 8 ~ "Hispanic"
  )) |>
  mutate(age_group = case_when(
    `_AGEG5YR` %in% sprintf("%02d", 1:5) ~ "18-44",
    `_AGEG5YR` %in% sprintf("%02d", 6:9) ~ "45-64",
    `_AGEG5YR` %in% sprintf("%02d", 10:13) ~ "65+",
    `_AGEG5YR` %in% 14 ~ "Missing"
  )) |>
  data.table() |>
  filter(!is.na(hbp)) |>
  filter(age_group != "Missing") |>
  filter(!is.na(urban))
  

c("_AGEG5YR",  #1-5: 18-44, 6-9: 45-64, 10-13:65 and older, 14: Missing
  "_SEX",     #1=Male, 2=Female
  "_RACE",     # 1=NH white, 2=NH black, 3,5,6,7,9=NH other, 4=NH Asian, 8=Hispanic
  "_RFHYPE6", # 1=No, 2=Yes, 9=MIssing
  "_URBSTAT") # Urban/Rural status 1-urban, 2-rural, 3-dont know