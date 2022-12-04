library('caret')
library('ROSE')
library('randomForest')
library('pROC')
library('dplyr')
library("Boruta")

Data <- ALL[,3:405]
Outcome <- ALL$Outcomes


# Mann-Whitney U  ---------------------------------------------------------
Pmw <- apply(Data,2,function(x)  wilcox.test(x ~ Outcome, paired = FALSE)$p.value) 

serial_pmw <- which(Pmw<0.05)                                   
Npmw <-  names(serial_pmw)    

Udata <- Data[Npmw]


# Spearman correlation ----------------------------------------------------
Udata <- Udata[, ! apply(Udata , 2 , function(x) sd(x, na.rm = TRUE)==0 ) ]
Cormatrix <- cor(Udata, y = NULL, use = "ev",method = "spearman")

High_correlate <- findCorrelation(
  Cormatrix,
  cutoff = 0.8,
  verbose = FALSE,
  names = FALSE,
  exact = TRUE
)

Irdata <- Udata[, -High_correlate]


# recursive feature elimination 100 iterations------------------------------
Top_feature <- matrix("", nrow = 100, ncol = 8)

for (i in 1:100) {
  ##split in trainig and validation
  set.seed(i)
  
  trainx <-
    createDataPartition(as.factor(Outcome),
                        times = 1,
                        p = 0.7,
                        list = FALSE)
  
  OutcomeT <- Outcome[trainx]
  OutcomeV <- Outcome[-trainx]

  DataT <- Irdata[trainx, ]
  DataV <- Irdata[-trainx, ]

  rfemodel <-
    rfeControl(
      functions = rfFuncs,
      method = "repeatedcv",
      repeats = 5,
      verbose = FALSE
    )
  rfefit <- rfe(DataT,as.factor(OutcomeT), rfeControl = rfemodel, size = 8)

  selectedData <- DataT[, rfefit$optVariables[1:8]]

  rfmodel <-
    randomForest(
      y = as.factor(dOutcomeT),
      x = as.data.frame(selectedData),
      na.action = na.exclude,
      mtry = 5
    )

  ForestPrediction <- predict(rfmodel, DataV[, selectedData$optVariables[1:8]], type = "prob")

  Top_feature[i,] <-selectedData$optVariables[1:8]
}

Top_feature_data <-as.vector(Top_feature, mode="any")

Features_freq <-count(Top_feature_data)
Top_features_freq <-Features_freq[order(Features_freq[,"n"], decreasing= T), ]

Selected_30 <- subset(Top_features_freq, n >= 30, select = c(Top_feature_data))
Top_features_name<- array(Selected_30$Top_feature_data)

Rfedata <- Irdata[Top_features_name]


# Boruta ------------------------------------------------------------------
set.seed(1)
boruta_method <-Boruta(Rfedata,as.factor(Outcome),pValue = 0.01,mcAdj = TRUE,maxRuns = 500,doTrace = 2,ntree =500,holdHistory = TRUE,getImp = getImpExtraZ)
plot(boruta_method)

Result_Brouta <- attStats(boruta_method)

Finaldata <- Rfedata[which(Result_Brouta$decision == "Confirmed")]