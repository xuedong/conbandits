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
simulateLinUcb = function(data,instance,job,doTimeDependentVerification,
                          batchSize, alpha0, mlrLearnerName = "noMlrLearner"){

  
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
  
  newBatch = c()
  for(instanceId in onlineLearnerDataLinUcb$onlineScenario$runtimeSet){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerDataLinUcb = selectAlgorithmWithLinUcbBatch(newBatch, onlineLearnerDataLinUcb, alpha0=alpha0)  
      onlineLearnerDataLinUcb = handleTimepointPerformanceLinUcb(onlineLearnerDataLinUcb)
      newBatch = list()
    }
  }
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerDataLinUcb = selectAlgorithmWithLinUcbBatch(newBatch, onlineLearnerDataLinUcb,alpha0=alpha0)  
    newBatch = list()
  }
  
  #Calculates the performance of the simulation and adds it to the onlineLearnerData object ($performanceInfo)
  onlineLearnerDataLinUcb = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerDataLinUcb) #from postProcessingHelpers
  onlineLearnerDataLinUcb = addVerificationPerformanceToLinUcbOnlineLearnerData(onlineLearnerDataLinUcb) 
  onlineLearnerDataLinUcb  = handleTimepointPerformanceLinUcb(onlineLearnerDataLinUcb)
  
  return(onlineLearnerDataLinUcb)
  
}  

#Initialise the A and b matrices, based on the training instances
#For each of the training instances, the performance of all algorithms is made available
initialiseLinUcbModelsBasedOnTrainingData = function(onlineLearnerDataLinUcb){
  availableAlgorithms = onlineLearnerDataLinUcb$onlineScenario$consideredAlgorithms
  trainingInst = onlineLearnerDataLinUcb$onlineScenario$trainingSet
  features = getFeatureValuesForInstList(trainingInst, onlineLearnerDataLinUcb$onlineScenario$consideredFeatures,
                                             onlineLearnerDataLinUcb$onlineScenario$aslibScenario)
  aslibScenario = onlineLearnerDataLinUcb$onlineScenario$aslibScenario
  
  res = linucb_initialization(arms = availableAlgorithms, instances = trainingInst , features = features, 
                        scenario = aslibScenario, getReward = getPerformances, nb = 0)
  
  onlineLearnerDataLinUcb$linUcbInfo$A = res$A
  onlineLearnerDataLinUcb$linUcbInfo$b = res$b
  
  return(onlineLearnerDataLinUcb)
    
}


#Function that handles one batch of the UCB algorithm
# Args:
#		instanceList: list of instances for which algorithms should be selected
#   onlineLearnerData.  An onlineLearnerData with additional structure 'called'onlineLearnerData$linUcbInfo'
#                       containing two sub-elements: A and b (the matrices required by the LinUCB algorithm)
#                       onlineLearnerData$amountOfStepsBeforeRetraining should be set to infinity for this code to work
#   alpha:  the value of parameter alpha
#
# Returns:
#  onlineLearnerData: the updated onlineLearnerData, containing the new A and b matrices
#                     and containing the selections for the instances in instanceList

selectAlgorithmWithLinUcbBatch = function(instanceList, onlineLearnerData, alpha0){
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
                               features = nextFeatures, alpha0 = alpha0, scenario = aslibScenario, getReward = getPerformances, nb = nrOfOnInstHandled)
  
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

#Adds the performance of the current models on the verification instances to the passed onlineLearnerDataLinUcb object
#as verification performance
#Should be called at the end of a simulation
addVerificationPerformanceToLinUcbOnlineLearnerData = function(onlineLearnerDataLinUcb){
  verInstances = onlineLearnerDataLinUcb$onlineScenario$verificationSet
  onlineLearnerDataLinUcb$performanceInfo$verification = getPerfOverviewOnInstSetLinUcb(onlineLearnerDataLinUcb, verInstances)
  return(onlineLearnerDataLinUcb)
}

#Calculates performance over time and adds it to the passed onlineLearnerDataLinUcb object
handleTimepointPerformanceLinUcb = function(onlineLearnerDataLinUcb){
  #Order is important for consequent updating of the list of timepoints! See addTimestepRegressionModelQualityInformationToOnlineLearnerData
  if(onlineLearnerDataLinUcb$doTimeDependentVerification){
    onlineLearnerDataLinUcb = addTimestepPerformanceToLinUcbOnlineLearnerData(onlineLearnerDataLinUcb)
  }
  #if(onlineLearnerDataLinUcb$doTimeDependentRegressionModelEvaluation){
  #not yet implemented
  #potential todo: write code to extract regression model quality for each of the algorithms
  #}
  return(onlineLearnerDataLinUcb)
}


#Adds the performance of the current models on the verification instances to the passed onlineLearnerDataLinUcb object
#for the current timestep
addTimestepPerformanceToLinUcbOnlineLearnerData = function(onlineLearnerDataLinUcb){
  currentTimestep = length(onlineLearnerDataLinUcb$instanceTimeMap) #Amount of handled online instances
  
  if(length(onlineLearnerDataLinUcb$performanceInfo$timestep$availableTimesteps) > 0){
    lastTimestepWithPerformanceInfo = tail(onlineLearnerDataLinUcb$performanceInfo$timestep$availableTimesteps,1)
  }
  else{
    lastTimestepWithPerformanceInfo = -1 #no timestep performance info yet
  }
  #Only if the new timestep is actually different from the last one is it added. 
  #(if not: the performance info will be overwritten)
  if(currentTimestep != lastTimestepWithPerformanceInfo){
    onlineLearnerDataLinUcb$performanceInfo$timestep$availableTimesteps = c(onlineLearnerDataLinUcb$performanceInfo$timestep$availableTimesteps, currentTimestep)
  }
  
  verInstances = onlineLearnerDataLinUcb$onlineScenario$verificationSet
  
  performanceOverviewDataTable =getPerfOverviewOnInstSetLinUcb(onlineLearnerDataLinUcb,verInstances)
  positionOfNewTimepoint = length(onlineLearnerDataLinUcb$performanceInfo$timestep$availableTimesteps)
  timepointInfo = performanceOverviewDataTable 
  onlineLearnerDataLinUcb$performanceInfo$timestep$selectionModelQualityList[[positionOfNewTimepoint]] = timepointInfo
  
  return(onlineLearnerDataLinUcb)
  
}

#Returns the performance of the selections made by LinUCB, when making purely greedy selections, on the passed instanceSet
getPerfOverviewOnInstSetLinUcb = function(onlineLearnerDataLinUcb, instanceSet){
  #1) input conversion
  A = onlineLearnerDataLinUcb$linUcbInfo$A
  b = onlineLearnerDataLinUcb$linUcbInfo$b
  availableAlgorithms = onlineLearnerDataLinUcb$onlineScenario$consideredAlgorithms
  nextFeatures = getFeatureValuesForInstList(instanceSet, onlineLearnerDataLinUcb$onlineScenario$consideredFeatures,
                                             onlineLearnerDataLinUcb$onlineScenario$aslibScenario)
  aslibScenario = onlineLearnerDataLinUcb$onlineScenario$aslibScenario
  
  #2. Calling LinUcb function
  res = linucb_predict(A = A, b = b, arms = availableAlgorithms , instances = instanceSet, 
                       features = nextFeatures, scenario = aslibScenario, getReward = getPerformances)
  
  #3) Transforming selection overview to acceptable selection overview format
  selectionOverview =makeDataFrame(nrow = length(instanceSet), ncol = 2, 
                                   col.types = c("character", "character") ,
                                   col.names = c("instanceId", "selectedAlgorithm"))
  
  selectionOverview[,1] = instanceSet
  counter = 1
  for(instance in instanceSet){
    selectedAlgorithm = availableAlgorithms[res$armChoices[counter]] #get algorithm selected for instance
    selectionOverview[[selectionOverview$instanceId == instance,2]] = selectedAlgorithm
    counter = counter+1
  }
  
  #Internal stupidity due to legacy code that requires another format for the selectedAlgorithmOverview
  
  resPerformance = createPerformanceOverviewForSelectionOverview(selectionOverview, onlineLearnerDataLinUcb$onlineScenario)
  
  return(resPerformance)
}


