library(arrow)
library(data.table)
library(ggplot2)
library(here)
library(tidyverse)
library(sf)
library(tigris)

dt <- open_dataset(here("data", "emory-limited-data-set-export"),format = "parquet")# %>% 
  # mutate(session_received_utc_dt = ymd_hms(session_started_local_time, tz = "America/New_York")) %>% 
  # mutate(year = year(session_received_utc_dt))

# Error in `compute.arrow_dplyr_query()`:
#   ! Invalid: Timestamp is ambiguous in timezone 'America/New_York': 2018-11-04 01:31:29 is ambiguous.  It could be
# 2018-11-04 01:31:29 EDT == 2018-11-04 05:31:29 UTC or
# 2018-11-04 01:31:29 EST == 2018-11-04 06:31:29 UTC

# Notice that 2018-11-04 is end of Daylights Savings. So need to change to timezone without DST?

#' zipcode
#' birth_year
#' gender
#' ethnicity
#' height_inches
#' weight_labs
#' bmi
#' bp_systolic
#' bp_diastolic
#' bp_map

# Store counts ------------------------------------------------------------
# dir.create(here("data", "counts"))

MY <- dt %>%
  mutate(year = substr(session_received_utc, 1, 4))%>%
  count(year, state) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  collect() 

ethnicity <- dt %>%
  count(ethnicity) %>%
  mutate(ethnicity = ifelse(ethnicity == "", NA, ethnicity)) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  collect()

zipcode <- dt %>%
  count(zipcode) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  collect()

gender <- dt %>%
  count(gender) %>%
  mutate(gender = ifelse(gender == "", NA, gender)) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  collect() 
  
birth_year <- dt %>%
  count(birth_year) %>%
  mutate(birth_year = as.numeric(ifelse(birth_year == "", NA, birth_year))) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  arrange(birth_year) %>%
  collect()

bp_systolic <- dt %>%
  count(bp_systolic) %>%
  mutate(bp_systolic = ifelse(bp_systolic == "", NA, bp_systolic)) %>%
  collect() %>%
  mutate(bp_systolic = as.numeric(bp_systolic)) %>%
  group_by(bp_systolic) %>%
  summarise(n=sum(n)) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  arrange(bp_systolic)

bp_diastolic <- dt %>%
  count(bp_diastolic) %>%
  mutate(bp_diastolic = ifelse(bp_diastolic == "", NA, bp_diastolic)) %>%
  collect() %>%
  mutate(bp_diastolic = as.numeric(bp_diastolic)) %>%
  group_by(bp_diastolic) %>%
  summarise(n=sum(n)) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  arrange(bp_diastolic)

bp_map <- dt %>%
  count(bp_map) %>%
  mutate(bp_map = ifelse(bp_map == "", NA, bp_map)) %>%
  collect() %>%
  mutate(bp_map = as.numeric(bp_map)) %>%
  group_by(bp_map) %>%
  summarise(n=sum(n)) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  arrange(bp_map)

bmi <- dt %>%
  count(bmi) %>%
  mutate(bmi = ifelse(bmi == "", NA, bmi)) %>%
  collect() %>%
  mutate(bmi = as.numeric(bmi)) %>%
  group_by(bmi) %>%
  summarise(n=sum(n)) %>%
  mutate(percent = n / nrow(dt)*100) %>%
  arrange(bmi)

# weight_lbs <- dt %>%
#   count(weight_lbs) %>%
#   mutate(weight_lbs = ifelse(weight_lbs == "", NA, weight_lbs)) %>%
#   collect() %>%
#   mutate(weight_lbs = as.numeric(weight_lbs)) %>%
#   group_by(weight_lbs) %>%
#   summarise(n=sum(n)) %>%
#   arrange(weight_lbs)
# 
# height_inches <- dt %>%
#   count(height_inches) %>%
#   mutate(height_inches = ifelse(height_inches == "", NA, height_inches)) %>%
#   collect() %>%
#   mutate(height_inches = as.numeric(height_inches)) %>%
#   group_by(height_inches) %>%
#   summarise(n=sum(n)) %>%
#   arrange(height_inches)

fwrite(MY, here("data", "counts", "month-year.csv"))
fwrite(ethnicity, here("data", "counts", "ethnicity.csv"))
fwrite(zipcode, here("data", "counts", "zipcode.csv"))
fwrite(gender, here("data", "counts", "gender.csv"))
fwrite(birth_year, here("data", "counts", "birth_year.csv"))
fwrite(bmi, here("data", "counts", "bmi.csv"))
fwrite(bp_systolic, here("data", "counts", "bp_systolic.csv"))
fwrite(bp_diastolic, here("data", "counts", "bp_diastolic.csv"))
fwrite(bp_map, here("data", "counts", "bp_map.csv"))

# Compute missingness -----------------------------------------------------

tf <- function(var){
  a <- get(var) %>%
    filter(is.na(get(var))) 
  
  pm <- ifelse(length(a$percent) == 0, 0, a$percent)
    data.frame(variable = var, percent_missing = pm)
}

vars <- c( "ethnicity", "zipcode", "gender", "birth_year",
          "bmi", "bp_systolic", "bp_diastolic", "bp_map")

df <- data.table::rbindlist(lapply(vars, tf))
fwrite(df, here("data", "counts", "percent_missing.csv"))

my <- data.table(MY)
my[, state := toupper(state)]
my <- my[, .(n = sum(n)), .(year, state)]
my <- my[nchar(state) == 2]

setnames(my, "state", "STUSPS")
# Includes DC and PR
state_sf <- tigris::states()
my <- right_join(state_sf, my, by= "STUSPS")
colsc <- function(...) {
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
    limits = range(..., na.rm = TRUE)
  )
}

plt <- ggplot() + 
  # geom_sf(data=st_geometry(my[!(my$STUSPS %in% c("AK", "HI", "PR")) & year == 2017,])) +
  geom_sf(data=st_geometry(state_sf[state_sf$STUSPS == "MA",])) + 
  # geom_sf(data = my[!(my$STUSPS %in% c("AK", "HI", "PR")) & my$year == 2017,], aes(fill=log(n)), col = "black") + 
  geom_sf(data = my[!(my$STUSPS %in% c("AK", "HI", "PR")) ,], aes(fill=log(n)), col = "black") + 
  facet_wrap(~year) + 
  theme_bw() + 
  colsc(seq(0,15.5,.01)) + 
  ggtitle("Number of sessions per state-year")
  
pdf(file = here("data", "counts", "map_state_year.pdf"), width=11, height=8.5)
print(plt)
dev.off()
