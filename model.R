library('randomForest')
library('caret')


# training cohort ---------------------------------------------------------
set.seed(1)
rfmodel <- randomForest(y = as.factor(OutcomeT), x = as.data.frame(DataT),ntree = 500, na.action = na.exclude)
plot(rfmodel)
varImpPlot(rfmodel)

ForestPredictionT <- predict(rfmodel,type = "prob")[,2]

ForestRocT <- roc(OutcomeT, rfmodel, ci=TRUE)
plot(ForestRocT)

ProbT <- predict(rfmodel)


# test cohorts ------------------------------------------------------------
set.seed(1)
ForestPredictionV <- predict(rfmodel,DataV,type = "prob")[,2]

ForestRocV <- roc(OutcomeV,ForestPredictionV,ci=TRUE)
plot(ForestRocV)

ProbV <- predict(rfmodel,DataV)
