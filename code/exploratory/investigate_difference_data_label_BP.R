library(arrow)
library(here)

path_sup <- here("data", "emory-limited-data-set-supplemental-export-v2")
dtsup    <- open_dataset( path_sup,format = "parquet") 

# Are all of the session IDs in high_blood_pressure_diagnosis_no in the other data label?
dt.yes <- dtsup |>
  filter(data_label_name == "high_blood_pressure_diagnosis_yes") |>
  count(session_id_mask) |> 
  select(session_id_mask) |>
  collect() 
dt.no <- dtsup |>
  filter(data_label_name == "high_blood_pressure_diagnosis_no") |>
  count(session_id_mask) |> 
  select(session_id_mask) |>
  collect() 

id.yes <- unique(dt.yes$session_id_mask)
id.no  <- unique(dt.no$session_id_mask)

mean(id.no %in% id.yes)
mean(id.yes %in% id.no)

# Resolved in email thread by Lauren