library(data.table)
library(here)
library(lme4)
library(merTools)
library(viridis)

# YEAR
# STATUS
# STAGE

# AGE GROUP
# ETHNICITY
# GENDER: (M or F)
# LEVEL: (COUNTY OR STATE)

# People only care about cascade for national level, but we can generate at county and state

fit <- readRDS(here("outputs", "glmer_2019-2020_prevalence_stage1.rds"))


YEAR_RANGES <- c("2017-2018", "2019-2020", "2021-2022", "2023-2024")


ps <- fread(here("data/poststratification_table.csv"), 
            colClasses = list(character = c("FIPS", "state_code")))

ps[, pred := predict(fit, ps, allow.new.levels = TRUE, type = "response")]

int <- predictInterval(fit, ps, level = 0.95)

ps[, weighted.mean(pred, n)]
ps[, weighted.mean(pred, n), by = age_group]
ps[, weighted.mean(pred, n), by = gender]
ps[, weighted.mean(pred, n), by = ethnicity]
ps[, weighted.mean(pred, n), by = urban]
ps[, weighted.mean(pred, n), by = .(age_group, ethnicity)]

a <- ps[, weighted.mean(pred, n), by = FIPS]
b <- ps[, weighted.mean(pred, n), by = state]

library(ggplot2)
library(tigris)
# Download county shapefiles
options(tigris_use_cache = TRUE) # Cache the shapefiles for faster plotting later
county_shapes <- counties(class = "sf", cb = TRUE) # cb = TRUE for simplified geometries
state_shapes <- states(class = "sf", cb = TRUE) 

# Merge your data with county shapefiles
merged_data <- county_shapes %>%
  left_join(a, by = c("GEOID" = "FIPS")) |>
  mutate(quintile = ntile(V1, 5))

ggplot(data = merged_data) +
  geom_sf(aes(fill = factor(quintile))) + # Use geom_sf to plot the map
  scale_fill_viridis(discrete = TRUE, option = "C", name = "Outcome") + # Viridis color scale
  theme_minimal() + # Minimal theme for a clean look
  geom_sf(data = state_shapes %>% filter(!(STATEFP %in% c("02", "15"))), fill = NA, color = "black", size = 0.2) + # State borders
  labs(title = "County-Level Outcome Choropleth Map",
       subtitle = "United States",
       caption = "Data source: Your data") +
  theme(axis.text = element_blank(), # Remove axis text
        axis.ticks = element_blank())  +# Remove axis ticks + 
coord_sf(xlim=c(-130, -60), ylim = c(20, 60))
