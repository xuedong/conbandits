simulateMlrLinUcb = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                          batchSize, mlrLearnerName, alpha0, minNrOfTrainingInst = 5){
  
  onlineScenario = instance  
  
  onlineLearnerData = doStandardPreProcessing(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = minNrOfTrainingInst,
                                              selectionFunction = selectBestAlgorithmBatch)
  
  
  newBatch = list()
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchLinUcb(newBatch, onlineLearnerData, alpha0)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData, selectionFunction = selectBestAlgorithmBatch)
      newBatch = list()
    }
  }
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchLinUcb(newBatch, onlineLearnerData, alpha0)  
    newBatch = list()
  }
  
  onlineLearnerData = doStandardPostProcessing(onlineLearnerData, selectionFunction = selectBestAlgorithmBatch)
  
  return(onlineLearnerData)
}

#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the 'pick predicted best' algorithm policy and updates the online data and regression models
handleInstanceBatchLinUcb =  function(newInstanceIdsList, onlineLearnerData, alpha0){
  selectedAlgorithmOverview = selectBestAlgorithmLinUcb(newInstanceIdsList, onlineLearnerData,alpha0)
  newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}

selectBestAlgorithmLinUcb = function(newInstanceIdsList, onlineLearnerData,alpha0){
  predictions = getBatchOfPredictedPerformances(newInstanceIdsList, onlineLearnerData)
  #featuresWithoutInstName = featuresForInstWithData[,2:ncol(featuresForInstWithData)]
  listOfDs = list()
  for(algorithm in onlineLearnerData$onlineScenario$consideredAlgorithms){
    instWithData = unlist(union(onlineLearnerData$onlineScenario$trainingSet, onlineLearnerData$onlineScenario$onlineInstanceOverview[[algorithm]]))
    featuresForInstWithData = getFeatureValuesForInstList(instWithData, onlineLearnerData$onlineScenario$consideredFeatures, 
                                                          onlineLearnerData$onlineScenario$aslibScenario)
    featuresWithoutInstName = featuresForInstWithData[,c(2:ncol(featuresForInstWithData))]
    listOfDs[[algorithm]] = matrix(unlist(featuresWithoutInstName), ncol = ncol(featuresWithoutInstName))
  }
  
  selectionOverview = list()
  for(instance in newInstanceIdsList){
    featValues = getFeatureValuesForInstList(instance, onlineLearnerData$onlineScenario$consideredFeatures, onlineLearnerData$onlineScenario$aslibScenario)
    predForInst = linucb_step(D = listOfDs, p= unlist(predictions[instance,]), arms = colnames(predictions),
                      instance = instance, feature = featValues, alpha0 = alpha0, 
                      scenario = onlineLearnerData$onlineScenario$aslibScenario, getReward = getPerformances)
    selectionOverview[[instance]] = colnames(predictions)[[predForInst$armChoice]]
  }
  
  
  return(selectionOverview)

  
}









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

aslibScenarioName = "CPMP-2015"
pInTraining = 0
pInRuntime = 0.9
pInVerification = 0.1
performanceMeasure = "PAR10"


nrOfStepsWithoutRetraining = 9
keepOldRegressionTasks = FALSE
doTimeDependentVerification = TRUE
doTimeDependentRegressionModelVerification = TRUE
batchSize = 10
mlrLearnerName = "regr.lm"
minNrOfTrainingInst = 0

onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName, pInTraining, pInRuntime, pInVerification, "PAR10")

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
                               batchSize = batchSize, alpha0=100)

mean(greedyResLin$performanceInfo$runtime$observedPerformance)
mean(linUcbTest$performanceInfo$runtime$observedPerformance)
mean(linUcbWithExploringTest$performanceInfo$runtime$observedPerformance)

