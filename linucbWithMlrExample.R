set.seed(10)



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
pInTraining = 0
pInRuntime = 0.9
pInVerification = 0.1
performanceMeasure = "PAR10"


nrOfStepsWithoutRetraining = 9
keepOldRegressionTasks = FALSE
doTimeDependentVerification = TRUE
doTimeDependentRegressionModelEvaluation = TRUE
batchSize = 10
mlrLearnerName = "regr.glmnet"
minNrOfTrainingInst = 5



onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName, pInTraining, pInRuntime, pInVerification, "PAR10")
instance = onlineScenario



set.seed(10)
greedyResLin = simulateGreedy(NULL, onlineScenario, NULL, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                              keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation, 
                              mlrLearnerName = mlrLearnerName, batchSize = batchSize)
set.seed(10)
linUcbTest = simulateMlrLinUcb(NULL, onlineScenario, NULL, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining,
                               keepOldRegressionTasks, doTimeDependentVerification,
                               doTimeDependentRegressionModelEvaluation, mlrLearnerName = mlrLearnerName,
                               batchSize = batchSize, alpha0=0)
set.seed(10)
linUcbWithExploringTest = simulateMlrLinUcb(NULL, onlineScenario, NULL, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining,
                                            keepOldRegressionTasks, doTimeDependentVerification,
                               doTimeDependentRegressionModelEvaluation, mlrLearnerName = mlrLearnerName,
                               batchSize = batchSize, alpha0=1)

mean(greedyResLin$performanceInfo$verification$virtualBest)
mean(greedyResLin$performanceInfo$verification$singleBest)

mean(greedyResLin$performanceInfo$verification$observedPerformance)
mean(linUcbTest$performanceInfo$verification$observedPerformance)
mean(linUcbWithExploringTest$performanceInfo$verification$observedPerformance)


