library(arrow)
library(data.table)
library(here)

pursuant   <- open_dataset(here("data", "kiosk-data-parquet-cleaned-4"),
                           format = "parquet") 

taskmap <- pursuant |> 
  count(state, year_range) |>
  collect() |>
  data.table()

fwrite(taskmap, here("data", "taskmap.csv"))
