library(data.table)
library(here)
library(lme4)
# library(broom)
# library(ciTools)
library(merTools)

# https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html

ps <- fread(here("data/reference/poststratification_table.csv"), 
            colClasses = list(character = "FIPS"))

outcome <- "controlled"
stage   <- "stage2"

model1 <- readRDS(here("results/models/glmer_2017-2020_awareness_conditional_stage2.rds"))
model2 <- readRDS(here("results/models/glmer_2021-2024_awareness_conditional_stage2.rds"))

model1 <- readRDS(here("results/models/glmer_2017-2020_controlled_conditional_stage1.rds"))
model2 <- readRDS(here("results/models/glmer_2021-2024_controlled_conditional_stage1.rds"))
a <- fixef(model1)
b <- fixef(model2)

dt <- cbind(a,b)
plot(dt)
abline(a=0,b=1)

# Fixed effects
# Random effects
# Predictions on PS table and see which ones are most extreme

ps[, m1 := predict(model1, newdata=ps, allow.new.levels = TRUE, type = "response")]
ps[, m2 := predict(model2, newdata=ps, allow.new.levels = TRUE, type = "response")]

ps[, mean(m1 - m2), .(age_group, ethnicity, gender)][order(V1)]


add_pi(ps, model1, allow.new.levels = TRUE)

ps[, weighted.mean(m1, n)]
ps[, weighted.mean(m2, n)]

bs <- bootMer(model1, FUN = function(x) predict(x, type = "response"), nsim = 10)

# Compare predictInterval to normal predict function
a <- merTools::predictInterval(model2, ps[1:100], seed = 123, type = "probability", 
                               returnSims = TRUE, n.sims = 100)
predict(model2, ps[1:100], type = "response")
