library(lme4)
library(pROC)



model <- readRDS("results/models/glmer_2017-2020_controlled_conditional_stage2.rds")
model <- readRDS("results/models/glmer_2017-2020_awareness_conditional_stage2.rds")
model <- readRDS("results/models/glmer_2017-2020_awareness_marginal_stage2.rds")

p <- predict(model, type = "response")
# y <- model.frame(model)[, 1]
y <- getME(model, "y")

auc <- pROC::roc(response = y, predictor = p)

print(auc$auc)
