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
source("LinUcbIntegrationInSimulation.R")

proportionTrainingInstances = 0.1
proportionRuntimeInstances = 0.8
proportionVerificationInstances = 0.1
keepOldRegressionTasks = FALSE
solvedInstancesOnly = FALSE
batchSize = 10
doTimeDependentVerification = TRUE
doTimeDependentRegressionModelEvaluation = FALSE  #not used for linUcb, but for the other methods it is used
nrOfStepsWithoutRetraining = 10 #not used for linUcb, but for the other methods it is used
nrOfOnlineInstancesToUseAsTrainingForOfflineMethod = 0
alpha0 = 0 #parameter for LinUCB
#mlrLearnerUcb = selectMlrLearner("LinUCB") #A bogus learner that doesn't do anything. Used because internal structure requires some notion of learner, but here the LinUCB algorithm itself contains the learning algorithm
mlrLearnerNameGreedy= "regr.randomForest"
performanceMeasure = "runtime" #PAR10 can be used too, to penalise time-outs by factor 10

aslibScenarioName = "QBF-2011"



#This is the code from the original test, adapted a little so it still works
set.seed(1)
onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName, pInTraining=proportionTrainingInstances, 
                                                     pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances, 
                                                     performanceMeasure = performanceMeasure)

onlineScenarioSample = onlineScenarioSubsampleFunction(data = onlineScenario, job =NULL, pInTraining = proportionTrainingInstances,
                                                       pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances,
                                                       performanceMeasure = performanceMeasure)

onlineLearnerDataLinUcb = initialiseEmptyOnlineLearnerData(onlineScenario = onlineScenarioSample, nrOfStepsWithoutRetraining = Inf,
                                                keepOldRegressionTasks = FALSE, doTimeDependentVerification = doTimeDependentVerification,
                                                doTimeDependentRegressionModelEvaluation = FALSE)

availableAlgorithms = onlineLearnerDataLinUcb$onlineScenario$consideredAlgorithms
desiredFeatures = onlineLearnerDataLinUcb$onlineScenario$consideredFeatures 
availableInstances = onlineLearnerDataLinUcb$onlineScenario$consideredInstances
initInstances = availableInstances[1:100]
nextInstances = availableInstances[101:500]
selInstances = availableInstances[501:600]
timeHorizon = length(availableInstances)
aslibScenario = onlineLearnerDataLinUcb$onlineScenario$aslibScenario #contains all info about the benchmark

features = getFeatureValuesForInstList(availableInstances, desiredFeatures, aslibScenario)
initFeatures = features[1:100,]
nextFeatures = features[101:500,]
selFeatures = features[501:600,]

res1 = linucb_disjoint(availableAlgorithms, initInstances, initFeatures, alpha0=alpha0, aslibScenario, getPerformances, 0)


res2 = linucb_disjoint_update(res1$A, res1$b, availableAlgorithms, nextInstances, nextFeatures, alpha0=alpha0, aslibScenario, getPerformances, 100)
init = linucb_initialization(availableAlgorithms, initInstances, initFeatures, aslibScenario, getPerformances, 0)

#Test on linucb_predict which makes selections based only on the underlying linear regression model of LinUCB
sels = linucb_predict(res2$A, res2$b, availableAlgorithms, selInstances, selFeatures, aslibScenario, getPerformances)




#This is the code for the new test. It compares LinUCB called in batches to a simple greedy method

set.seed(1)
onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName, pInTraining=proportionTrainingInstances, 
                                                     pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances, 
                                                     performanceMeasure = performanceMeasure)

onlineScenarioSample = onlineScenarioSubsampleFunction(data = onlineScenario, job =NULL, pInTraining = proportionTrainingInstances,
                                                       pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances,
                                                       performanceMeasure = performanceMeasure)


#Simulation 1: LinUCB with batches
onlineLearnerDataLinUcbBatchedRes = simulateLinUcb(data = NULL, instance = onlineScenarioSample, job =  NULL,
                           doTimeDependentVerification = doTimeDependentVerification,
                           batchSize = batchSize , alpha0 = alpha0)


#Simulation 2: LinUCB without batches
onlineLearnerDataLinUcbNotBatchedRes = simulateLinUcb(data = NULL, instance = onlineScenarioSample, job =  NULL,
                                                      doTimeDependentVerification = doTimeDependentVerification,
                                                      batchSize = Inf , alpha0 = alpha0)

#Simulating 3: greedy method that uses underlying regression models (takes much longer to run. A few minutes)
resGreedy = simulateGreedy(data = NULL,instance = onlineScenarioSample,NULL,nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                           keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification, 
                           doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation,  batchSize = batchSize, 
                           mlrLearnerName = mlrLearnerNameGreedy)


#Comparing performances
greedyAvgPerf = mean(resGreedy$performanceInfo$runtime$observedPerformance)
linUcbBatchedPerf = mean(onlineLearnerDataLinUcbBatchedRes$performanceInfo$runtime$observedPerformance)
linUcbNotBatchedPerf = mean(onlineLearnerDataLinUcbNotBatchedRes$performanceInfo$runtime$observedPerformance)
sbsPerf = mean(onlineLearnerDataLinUcbNotBatchedRes$performanceInfo$runtime$singleBest)
vbsPerf = mean(onlineLearnerDataLinUcbNotBatchedRes$performanceInfo$runtime$virtualBest)

print("Performances: (Higher is better)")

print(paste("alpha0: ", alpha0))
print(paste("greedy perf: ", greedyAvgPerf))
print(paste("batched linUcb perf: ", linUcbBatchedPerf))
print(paste("not batched linUcb perf: ", linUcbNotBatchedPerf))
print(paste("single best solver perf: ", sbsPerf))
print(paste("virtual best solver perf: ", vbsPerf))





