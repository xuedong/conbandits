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

proportionTrainingInstances = 0.1
proportionRuntimeInstances=0.8
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


#Testing LinUCB
set.seed(10)

tStartLinUcb = Sys.time()
linUcbRes = simulateLinUcb(data = NULL, instance = onlineScenarioTest, job =  NULL,
                                                   doTimeDependentVerification = doTimeDependentVerification,
                                                   batchSize = batchSize , alpha0 = alpha0)
tEndLinUcb = Sys.time()
print(tEndLinUcb-tStartLinUcb)



#testing greedy with linear regr model
set.seed(10)
tStartLin = Sys.time()
greedyResLin = simulateGreedy(NULL, onlineScenarioTest, NULL, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation, mlrLearnerName = linRegrName, batchSize = batchSize)
tEndLin = Sys.time()
print(tEndLin-tStartLin)

#testing greedy with random forest regr model
set.seed(10)
tStartRF= Sys.time()
set.seed(10)
greedyResRf = simulateGreedy(NULL, onlineScenarioTest, NULL, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation, mlrLearnerName = regForestName, batchSize = batchSize)
tEndRF= Sys.time()
print(tEndRF-tStartRF)







#comparing results
print("Online performance")
greedyLinOn = mean(greedyResLin$performanceInfo$runtime$observedPerformance)
greedyRfOn = mean(greedyResRf$performanceInfo$runtime$observedPerformance)
linUcbOn = mean(linUcbRes$performanceInfo$runtime$observedPerformance)
sbsOn = mean(linUcbRes$performanceInfo$runtime$singleBest)
vbsOn = mean(linUcbRes$performanceInfo$runtime$virtualBest)
print(paste("Sbs ", sbsOn))
print(paste("Vbs ", vbsOn))
print(paste("greedy Linear regr ", greedyLinOn))
print(paste("greedy Random forest ", greedyRfOn))
print(paste("linUcb ", linUcbOn))



print("Verification performance")
greedyLinVer = mean(greedyResLin$performanceInfo$verification$observedPerformance)
greedyRfVer = mean(greedyResRf$performanceInfo$verification$observedPerformance)
linUcbVer = mean(linUcbRes$performanceInfo$verification$observedPerformance)
sbsVer = mean(linUcbRes$performanceInfo$verification$singleBest)
vbsVer = mean(linUcbRes$performanceInfo$verification$virtualBest)
print(paste("Sbs ", sbsVer))
print(paste("Vbs ", vbsVer))
print(paste("greedy Linear regr ", greedyLinVer))
print(paste("greedy Random forest ", greedyRfVer))
print(paste("linUcb ", linUcbVer))



#Creating plots







