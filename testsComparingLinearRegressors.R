library(aslib)
library(mlr)
library(llama)

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("helpFunctions")
source("LinUcbIntegrationInSimulation.R")
source("utils_disjoint.R")
source("linucb_disjoint.R")

proportionTrainingInstances = 0.9
proportionRuntimeInstances=0.0
proportionVerificationInstances = 0.1
nrOfStepsWithoutRetraining = 10
keepOldRegressionTasks = FALSE
solvedInstancesOnly = FALSE
batchSize = 10
lcbLambda = 1
doTimeDependentRegressionModelEvaluation = FALSE
doTimeDependentVerification = TRUE

linRegrName = "regr.lm"
regForestName = "regr.randomForest"

performanceMeasure = "runtime"
aslibScenarioName = "QBF-2011"

alpha0 = 0 #parameter for LinUCB



set.seed(10)
onlineScenarioTest = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName, pInTraining=proportionTrainingInstances, 
                                                         pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances,
                                                         performanceMeasure = performanceMeasure)

instance = onlineScenarioTest



onlineScenario = instance  

#keepOldRegressionTasks has no meaning for linUcB; it is a concept of MLR. 
#doTimedependentRegressionVerification is set to False, as this has not yet been implemented for LinUCB
#nrOfStepsWithoutRetraining is set to infinity, because the code used for retraining assumes Mlr models, which are not present
#for LinUCB. In this way, retraining is never attempted.
onlineLearnerDataLinUcb = initialiseEmptyOnlineLearnerData(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = Inf,
                                                           keepOldRegressionTasks = FALSE, doTimeDependentVerification = doTimeDependentVerification,
                                                           doTimeDependentRegressionModelEvaluation = FALSE)


onlineLearnerDataLinUcb$nrOfStepsWithoutRetraining = Inf #See remark at the end of selectAlgorithmWithLinUcbBatch
#Initialising the A and b matrix based on the training data
onlineLearnerDataLinUcb = initialiseLinUcbModelsBasedOnTrainingData(onlineLearnerDataLinUcb)  

#Add performance of the models before the simulation has started
onlineLearnerDataLinUcb  = handleTimepointPerformanceLinUcb(onlineLearnerDataLinUcb)


mlrLearnerName = "regr.penalized.ridge"


onlineScenarioRidge = instance  

onlineLearnerDataRidge = doStandardPreProcessing(onlineScenario = onlineScenarioRidge, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                            keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                            doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                            mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = 5,
                                            selectionFunction = selectBestAlgorithmBatch)


mlrLearnerName = "regr.lm"


onlineScenarioLS = instance  

onlineLearnerDataLS = doStandardPreProcessing(onlineScenario = onlineScenarioLS, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                            keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                            doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                            mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = 5,
                                            selectionFunction = selectBestAlgorithmBatch)

mlrLearnerName = "regr.randomForest"

onlineScenarioRF = instance  

onlineLearnerDataRF = doStandardPreProcessing(onlineScenario = onlineScenarioRF, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = 5,
                                              selectionFunction = selectBestAlgorithmBatch)




print("observed performances")
paste("ridge", mean(onlineLearnerDataRidge$performanceInfo$timestep$selectionModelQualityList[[1]]$observedPerformance))
     paste("single best",mean( onlineLearnerDataRidge$performanceInfo$timestep$selectionModelQualityList[[1]]$singleBest))
     paste("oracle",mean( onlineLearnerDataRidge$performanceInfo$timestep$selectionModelQualityList[[1]]$virtualBest))
     paste("linucb",mean( onlineLearnerDataLinUcb$performanceInfo$timestep$selectionModelQualityList[[1]]$observedPerformance))
     paste("linear regression least",mean( onlineLearnerDataLS$performanceInfo$timestep$selectionModelQualityList[[1]]$observedPerformance))
paste("random forest ", mean(onlineLearnerDataRF$performanceInfo$timestep$selectionModelQualityList[[1]]$observedPerformance))

mean(onlineLearnerDataLinUcb$performanceInfo$timestep$selectionModelQualityList[[1]]$singleBest)
mean(onlineLearnerDataLinUcb$performanceInfo$timestep$selectionModelQualityList[[1]]$virtualBest)



