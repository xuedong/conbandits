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


#Function that handles one batch of the UCB algorithm
# Args:
#		instanceList: list of instances for which algorithms should be selected
#   onlineLearnerData.  An onlineLearnerData with additional structure 'called'onlineLearnerData$linUcbInfo'
#                       containing two sub-elements: A and b (the matrices required by the LinUCB algorithm)
#   alpha:  the value of parameter alpha
#
# Returns:
#  onlineLearnerData: the updated onlineLearnerData, containing the new A and b matrices
#                     and containing the selections for the instances in instanceList

selectAlgorithmWithLinUcbBatch = function(instanceList, onlineLearnerData, alpha){
  
  #1. Transforming data from onlineLearnerData format to linUcb algorithm format
  A = onlineLearnerData$linUcbInfo$A
  b = onlineLearnerData$linUcbInfo$b
  availableAlgorithms = onlineLearnerData$onlineScenario$consideredAlgorithms
  nextInstances = instanceList
  nextFeatures = getFeatureValuesForInstList(instanceList, onlineLearnerData$onlineScenario$consideredFeatures,
                                             onlineLearnerData$onlineScenario$aslibScenario)
  aslibScenario = onlineLearnerData$onlineScenario$aslibScenario
  nrOfOnInstHandled = length(onlineLearnerData$instanceTimeMap) #instanceTimeMap contains all instances that have been handled before during the simulation (in order)
  
  #2. Calling LinUcb function
  res = linucb_disjoint_update(A = A, b = b, arms = availableAlgorithms, instances =  nextInstances,
                               features = nextFeatures, alpha = alpha, scenario = aslibScenario, getReward = getRuntimes, nb = nrOfOnInstHandled)
  
  #3. Transforming updated LinUcb matrices/vectors back to onlineLearnerData
  onlineLearnerData$linUcbInfo$A = res$A
  onlineLearnerData$linUcbInfo$b = res$b
  
  #4. Storing the selected algorithms in the onlineLearnerData object
  counter = 1
  selectedAlgorithmOverview = list() #a list with as keys instances and as values the selected algorithm for the instance
  for(instance in instanceList){
    selectedAlgorithm = availableAlgorithms[res$armChoices[counter]] #get algorithm selected for instance
    selectedAlgorithmOverview[instance] = selectedAlgorithm
    counter = counter+1
  }
  
  #Internal stupidity due to legacy code that requires another format for the selectedAlgorithmOverview
  newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectedAlgorithmOverview)
  
  #This actually adds all new selected algorithms to the onlineLearnerData object. 
  #Note:  this function is also called by the regression-based methods for which the simulation was originally written
  #       Because of this, one of the steps it performs is retraining the regression models.
  #       LinUCB does not use any separate regression models, so this step makes no sense
  #       To avoid it is ever attempted, parameter 'amountOfStepsBeforeRetraining' is set to infinity
  #       the result is that it is never attempted to retrain the (not existing) regression models
  #       meaning that the code can execute successfully
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, instanceList, selectedAlgorithmOverview,newSelectionData )
  
  return(onlineLearnerData)
  
}


#Function that simulates the LinUCB algorithm on all specified instances, splitting them into batches of size batchSize
#		instancesToHandle: list of instances on whihc LinUCB should be run
#   onlineLearnerDataLinUcb.  An onlineLearnerData with additional structure 'onlineLearnerData$linUcbInfo'
#                       containing two sub-elements: A and b (the matrices required by the LinUCB algorithm)
#   alpha:  the value of parameter alpha
#   batchSize: size of batches in which the instances should be split
#
# Returns:
#  onlineLearnerData: the updated onlineLearnerData, containing the final A and b matrices ($linUcbInfo),
#                     information about the instances handled and the selections made ($onlineScenario$onlineInstanceOverview)
#                     and information about the performance of the method  ($performanceInfo)

doLinUcbSimulation = function(instancesToHandle, onlineLearnerDataLinUcb, alpha, batchSize){
  #Do LinUCB simulation
  newBatch = c()
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerDataLinUcb = selectAlgorithmWithLinUcbBatch(newBatch, onlineLearnerDataLinUcb, alpha=alpha)  
      #onlineLearnerDataLinUcb = handleTimepointPerformance(onlineLearnerDataLinUcb)
      newBatch = list()
    }
  }
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerDataLinUcb = selectAlgorithmWithLinUcbBatch(newBatch, onlineLearnerDataLinUcb,alpha=alpha)  
    newBatch = list()
  }
  
  #Calculates the performance of the simulation and adds it to the onlineLearnerData object ($performanceInfo)
  onlineLearnerDataLinUcb = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerDataLinUcb) #from postProcessingHelpers
  
}




sourceDir("helpFunctions")
source("utils_disjoint.R")
source("linucb_disjoint.R")


proportionTrainingInstances = 0.1
proportionRuntimeInstances = 0.8
proportionVerificationInstances = 0.1
nrOfStepsWithoutRetraining = 10
keepOldRegressionTasks = FALSE
solvedInstancesOnly = FALSE
batchSize = 10
doTimeDependentRegressionModelEvaluation = FALSE
doTimeDependentVerification = FALSE
nrOfOnlineInstancesToUseAsTrainingForOfflineMethod = 0
alpha = 0
mlrLearnerUcb = selectMlrLearner("LinUCB") #A bogus learner that doesn't do anything. Used because internal structure requires some notion of learner, but here the LinUCB algorithm itself contains the learning algorithm
mlrLearnerNameGreedy= "regr.randomForest"
performanceMeasure = "runtime" #PAR10 can be used too, to penalise time-outs by factor 10

aslibScenarioName = "QBF-2011"



set.seed(1)
onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName, pInTraining=proportionTrainingInstances, 
                                                      pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances, 
                                                      performanceMeasure = performanceMeasure)

onlineScenarioSample = onlineScenarioSubsampleFunction(data = onlineScenario, job =NULL, pInTraining = proportionTrainingInstances,
                                                          pInRuntime = proportionRuntimeInstances, pInVerification = proportionVerificationInstances,
                                                        performanceMeasure = performanceMeasure, mlrLearnerName = mlrLearnerNameGreedy)

onlineLearnerDataLinUcb = initialiseEmptyOnlineLearnerData(onlineScenario = onlineScenarioSample, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining,
                                                keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                                doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation)

#This is the code from the original test, adapted a little so it still works
availableAlgorithms = onlineLearnerDataLinUcb$onlineScenario$consideredAlgorithms
desiredFeatures = onlineLearnerDataLinUcb$onlineScenario$consideredFeatures 
availableInstances = onlineLearnerDataLinUcb$onlineScenario$consideredInstances
initInstances = availableInstances[1:100]
nextInstances = availableInstances[101:500]
timeHorizon = length(availableInstances)
aslibScenario = onlineLearnerDataLinUcb$onlineScenario$aslibScenario #contains all info about the benchmark

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



#This is the code for the new test. It compares LinUCB called in batches to a simple greedy method

#Updating the onlineLearnerData object so it contains the info required by LinUCB
onlineLearnerDataLinUcb$linUcbInfo$A = init$A
onlineLearnerDataLinUcb$linUcbInfo$b = init$b
onlineLearnerDataLinUcb$nrOfStepsWithoutRetraining = Inf #See remark at the end of selectAlgorithmWithLinUcbBatch
instancesToHandle = onlineScenarioSample$runtimeSet

#Doing the simulation (in batches)

#Simulation 1: LinUCB with batches
onlineLearnerDataLinUcbBatchedRes = doLinUcbSimulation(instancesToHandle = instancesToHandle, onlineLearnerDataLinUcb = onlineLearnerDataLinUcb, 
                   alpha = alpha, batchSize = batchSize  )

#Simulation 2: LinUCB without batches
onlineLearnerDataLinUcbNotBatchedRes = doLinUcbSimulation(instancesToHandle = instancesToHandle, onlineLearnerDataLinUcb = onlineLearnerDataLinUcb, 
                                                       alpha = alpha, batchSize = Inf  )

#Simulating the greedy method that uses underlying regression models (takes much longer to run. A few minutes)
resGreedy = simulateGreedy(data = NULL,instance = onlineScenarioSample,NULL,nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                           keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification, 
                           doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation,  batchSize = batchSize, 
                           mlrLearnerName = mlrLearnerNameGreedy)


#Comparing performances
greedyAvgPerf = mean(resGreedy$performanceInfo$runtime$observedPerformance)
linUcbBatchedPerf = mean(onlineLearnerDataLinUcbBatchedRes$performanceInfo$runtime$observedPerformance)
linUcbNotBatchedPerf = mean(onlineLearnerDataLinUcbNotBatchedRes$performanceInfo$runtime$observedPerformance)

print("Performances: (Higher is better)")

print(paste("greedy perf: ", greedyAvgPerf))
print(paste("batched linUcb perf: ", linUcbBatchedPerf))
print(paste("not batched linUcb perf: ", linUcbNotBatchedPerf))

