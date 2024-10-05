# Filter the supplemental data set down to only hypertension --------
library(arrow)
library(dplyr)
library(data.table)
library(here)

path_sup <- here("data", "emory-limited-data-set-supplemental-export-Sept24")
dtsup    <- open_dataset( path_sup,format = "parquet") 

# dtsup |>
#   filter(data_label_name %in% c("high_blood_pressure_diagnosis_yes")) |>
#   summarise(max(captured)) |>
#   collect()

dt <- dtsup |>
  filter(data_label_name %in% c("high_blood_pressure_diagnosis_yes")) |>
  collect()  |>
  data.table()

dt[, value := as.numeric(value)]
dt[, datetime := as.POSIXct(captured)]
dt <- dt[order(datetime)]

dt.final <- dt[dt[, .I[1], session_id_mask]$V1]
setnames(dt.final, "value", "hbp_diagnosis")
fwrite(dt.final[, .(session_id_mask, hbp_diagnosis)], "data/supplemental_hbp_cleaned_Sept24.csv")

# dt.no[, c("date", "time") := tstrsplit(captured, " ", fixed = TRUE)]
# dt.no[, date := as.Date(date)]