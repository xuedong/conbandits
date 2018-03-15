#sanity check of the regression model: training on 90% and testing on 10%

library(dplyr)
library(mlr)
library(llama)
library(aslib)
library(dplyr)
library(MASS)


#configureMlr(on.par.without.desc = "warn")

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("helpFunctions")
source("utils_disjoint.R")
#source("linucb_disjoint.R")
source("linucb_mlr.R")

aslibScenarioName = "QBF-2011"
#the instances in training and runtime set are combined within 'simulateOffline' and are all used for training
pInTraining = 0
pInRuntime = 0.9
pInVerification = 0.1
performanceMeasure = "PAR10"

keepOldRegressionTasks = FALSE
doTimeDependentVerification = TRUE
doTimeDependentRegressionModelEvaluation = TRUE
minNrOfTrainingInst = 5


onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName, pInTraining, pInRuntime, pInVerification, "PAR10")
instance = onlineScenario


#Copy of the getMlrNetLearner function to enable faster parameter tuning. Original is in helpFunctions/mlrLearnerMethodsInterface
getGlmNetLearner = function(predictionType, family = c("gaussian"), thresh=1e-10,alpha=0, standardize=FALSE, intercept=FALSE ){
  learnerLR = makeImputeWrapper(learner = makeLearner("regr.glmnet",predict.type=predictionType, thresh=thresh, alpha=alpha, standardize=FALSE, intercept=FALSE), 
                                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
                                               factor = imputeConstant("NA"), character = imputeConstant("NA")))
  
}

#uncomment below to create a glmnet with standard parameters => good performance
#getGlmNetLearner = function(predictionType){
#  learnerLR = makeImputeWrapper(learner = makeLearner("regr.glmnet",predict.type=predictionType), 
#                                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
#                                               factor = imputeConstant("NA"), character = imputeConstant("NA")))
#  
#}



#1) glmnet
mlrLearnerName = "regr.glmnet"

set.seed(10)
offlineRes = simulateOffline(NULL, onlineScenario, NULL, pOnlineAsTraining = 1, 
                             keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation, 
                             mlrLearnerName = mlrLearnerName)
mean(offlineRes$performanceInfo$verification$observedPerformance)
mean(offlineRes$performanceInfo$verification$singleBest)
mean(offlineRes$performanceInfo$verification$virtualBest)





#2) standard linear regression
mlrLearnerName = "regr.lm"

set.seed(10)
offlineRes = simulateOffline(NULL, onlineScenario, NULL, pOnlineAsTraining = 1, 
                             keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation, 
                             mlrLearnerName = mlrLearnerName)
mean(offlineRes$performanceInfo$verification$observedPerformance)
mean(offlineRes$performanceInfo$verification$singleBest)
mean(offlineRes$performanceInfo$verification$virtualBest)

