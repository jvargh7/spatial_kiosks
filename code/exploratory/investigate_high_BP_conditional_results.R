library(arrow)
library(data.table)
library(ggplot2)
library(here)

# Identify which states saw an increase in controlled hypertension from 2017-2018 to 2023-2024

full <- open_dataset(here("data", "raw", "emory-limited-data-set-export-Sept24"), format = "parquet") |> 
    mutate(year = substr(session_received_utc, 1, 4),
           bp_systolic = as.numeric(bp_systolic),
           bp_diastolic = as.numeric(bp_diastolic)) 
  
full.summary <- full |>
  group_by(year) |>
  summarise(`Avg. Systolic BP (mmHg)` = mean(bp_systolic),
            `Avg. Diastolic BP (mmHg)` = mean(bp_diastolic),
            `Percent of Observations (%)` = n() / nrow(full) * 100) |>
  collect()|>
  melt(id.vars = "year") |>
  mutate(type = "Full Data (N = 91.9 million)")
  
# SBP/DBP over time by year
dt <- fread(here("data/processed/high_quality_dt_after_deduplication_w_cov_Sept24.csv"), 
            colClasses = list(character = "FIPS"))

# Did the higher blood pressure apply to all sessions? 
dt.summary <- dt[, .(`Avg. Systolic BP (mmHg)` = mean(sbp), 
            `Avg. Diastolic BP (mmHg)` = mean(dbp),
            `Percent of Observations (%)` = 100 * .N / nrow(dt)), year]
            # hbp_diagnosis = mean(hbp_diagnosis),
            # hbp_stage1 = mean(hbp_stage1),
            # hbp_stage2 = mean(hbp_stage2),
            # hbp_bp_stage1 = mean(hbp_bp_stage1),
            # hbp_bp_stage2 = mean(hbp_bp_stage2)), year][order(year)]

dt.summary <- dt.summary |>
  melt(id.vars = "year") |>
  mutate(type = "Subset (N = 1.27 million)")

plot.dt <- rbind(full.summary, dt.summary)

plt <- ggplot(plot.dt[,], aes(year, value)) + 
  geom_point(aes(col = type)) + 
  geom_line(aes(group = type, col = type)) + 
  facet_wrap(~variable, scales = "free") + 
  theme_bw() + 
  theme(legend.position = "bottom") 


dt[, mean(sbp), .(year, state_region)][order(state_region, year)]

ggsave(here("results", "figures", "exploratory", "Trend in Avg. Blood Pressure by Year in Pursuant Data.png"), 
       width = 11, height = 6)
