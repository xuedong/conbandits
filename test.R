library(dplyr)
library(mlr)
library(llama)
library(aslib)

sourceDir <- function(path, trace = TRUE, ...) {
		for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
					if(trace) cat(nm,":")
		source(file.path(path, nm), ...)
			    if(trace) cat("\n")
			}
}
sourceDir("helpFunctions")
source("utils_disjoint.R")
source("linucb_disjoint.R")


proportionTrainingInstances = 0.1
proportionRuntimeInstances = 0.8
proportionVerificationInstances = 0.1
mlrLearnerConstant = getRandomForestLearner()
nrOfStepsWithoutRetraining = 10
keepOldRegressionTasks = FALSE
solvedInstancesOnly = FALSE
batchSize = 10
lcbLambda = 1
dTimeDependentRegressionModelEvaluation = FALSE
doTimeDependentVerification = FALSE
nrOfOnlineInstancesToUseAsTrainingForOfflineMethod = 0
alpha = 2


aslibScenarioName = "QBF-2011"

set.seed(1)
onlineScenarioTest = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName, pInTraining=proportionTrainingInstances, 
                                                         pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances)

onlineLearnerData = initialiseOnlineLearnerData(onlineScenario = onlineScenarioTest, mlrLearner = mlrLearnerConstant, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = TRUE, doTimeDependentRegressionModelEvaluation = TRUE)

availableAlgorithms = onlineLearnerData$onlineScenario$consideredAlgorithms
desiredFeatures = onlineLearnerData$onlineScenario$consideredFeatures 
availableInstances = onlineLearnerData$onlineScenario$consideredInstances
initInstances = availableInstances[1:100]
nextInstances = availableInstances[101:500]
timeHorizon = length(availableInstances)
aslibScenario = onlineLearnerData$onlineScenario$aslibScenario #contains all info about the benchmark

#instance = availableInstances[1]
#featureValues = getFeatureValuesForInstList(instance, desiredFeatures, aslibScenario)
#runtimeOfFirstAlgOnFirstInst = getRuntimes(availableAlgorithms[1], instance, aslibScenario) 
#runtimeOfFirstAlgOnAllInst = getRuntimes(availableAlgorithms[1], availableInstances, aslibScenario)

features = getFeatureValuesForInstList(availableInstances, desiredFeatures, aslibScenario)
initFeatures = features[1:100,]
nextFeatures = features[101:500,]

res1 = linucb_disjoint(availableAlgorithms, initInstances, initFeatures, alpha, aslibScenario, getRuntimes, 0)
res2 = linucb_disjoint_update(res1$A, res1$b, availableAlgorithms, nextInstances, nextFeatures, alpha, aslibScenario, getRuntimes, 100)
init = linucb_initialization(availableAlgorithms, initInstances, initFeatures, aslibScenario, getRuntimes, 0)

