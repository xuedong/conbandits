#Example for algorithm selection
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


proportionTrainingInstances = 0.1
proportionRuntimeInstances=0.8
proportionVerificationInstances = 0.1
mlrLearnerConstant = getRandomForestLearnerJackknife()
nrOfStepsWithoutRetraining = 10
keepOldRegressionTasks = FALSE
solvedInstancesOnly = FALSE
batchSize = 10
lcbLambda = 1
dTimeDependentRegressionModelEvaluation = FALSE
doTimeDependentVerification = FALSE
nrOfOnlineInstancesToUseAsTrainingForOfflineMethod = 0


aslibScenarioName = "QBF-2011"

set.seed(1)
onlineScenarioTest = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName, pInTraining=proportionTrainingInstances, 
                                                         pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances)

onlineLearnerData = initialiseOnlineLearnerData(onlineScenario = onlineScenarioTest, mlrLearner = mlrLearnerConstant, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = TRUE, doTimeDependentRegressionModelEvaluation = TRUE)

instance = onlineLearnerData$onlineScenario$consideredInstances[1]
availableAlgorithms = onlineLearnerData$onlineScenario$consideredAlgorithms
desiredFeatures = onlineLearnerData$onlineScenario$consideredFeatures 


featureValues = getFeatureValuesForInstList(instance, desiredFeatures, onlineLearnerData$onlineScenario$aslibScenario)

for(instance in onlineLearnerData$onlineScenario$consideredInstances){
  print(paste("handling instance", instance))
  features = getFeatureValuesForInstList(instance)
  print(features)
}        

