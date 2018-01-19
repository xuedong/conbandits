#bCreates a dataframe with the necessary data to create a plot
#Considers all experiments specified in expeirmentNrs. 
#Note that all experiments specified should be repetitions of the same expeirment for the results to make sense
createPlotDataFrameForPerfomranceOverviewOverTime = function(reg, experimentNrs, normalised=FALSE){
  resPerformance = makeDataFrame(nrow = 1, ncol = 16, 
                                 col.types = c("numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "character", "numeric", "numeric", "logical", "logical", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                                 col.names = c("expNr", "problemId", "pTrain", "pOnline", "pVerification", "nrOfInstances", "algorithmId", "repl", "timestep", "verification", "normalised", "modelRtraingFreq", "PAR10", "avgMisclassificationPenalty", "pSuccess", "PAR1"))
  VERIFICATION_CONSTANT = TRUE #Time dependent performance is always measured on the verification set
  rowCounter=1
  for(jobId in experimentNrs){
    jobInfo = getJobTable(reg = reg, ids=jobId)
    result = loadResult(reg=reg, jobId)
    
    timepoints = result$performanceInfo$timestep$availableTimesteps
    for(i in 1:length(timepoints)){
      if(normalised){
        vbsPerformance = result$performanceInfo$timestep$selectionModelQualityList[[i]]$vbsPerformance
        singleBestPerformance = result$performanceInfo$timestep$selectionModelQualityList[[i]]$singleBestPerformance
        observedPerformance = result$performanceInfo$timestep$selectionModelQualityList[[i]]$observedPerformance
        normalisedPerformance = getNormalisedPerformance(observedPerf = observedPerformance, singleBestPerf  =singleBestPerformance, vbsPerf = vbsPerformance)
        performanceValues = normalisedPerformance
      }
      else{
        performanceValues = result$performanceInfo$timestep$selectionModelQualityList[[i]]$observedPerformance
      }
      expNrValue = jobInfo$job.id
      
      #performance = performanceValues$selectionModelQualityList[[i]]
      resPerformance[rowCounter,] = list(expNr = expNrValue, problemId = toString(jobInfo$problem), pTrain = result$onlineScenario$pInTraining, pOnline = result$onlineScenario$pInRuntime, 
                                         pVerification = result$onlineScenario$pInVerification, nrOfInstances = length(result$onlineScenario$consideredInstances),   algorithmId = toString(jobInfo$algorithm), repl = jobInfo$repl, timestep = timepoints[[i]],
                                         verification = VERIFICATION_CONSTANT, normalised = normalised, rmodelRtraingFreq = result$nrOfStepsWithoutRetraining,
                                         PAR10 = performanceValues$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$averageMisclassificationPenalty, pSuccess = performanceValues$proportionOfSuccesses, PAR1 = performanceValues$meanPar1Score) 
      rowCounter = rowCounter+1
      
    }
    
   
  }
  
  return(resPerformance)
}

#Obtains a performance datapoint for each timestep
obtainPerformanceOverviewOverTime = function(onlineLearnerData, timestepSize){
  nrOfOnlineInstances = length(onlineLearnerData$onlineScenario$runtimeSet)
  performanceList = list()
  timepointList = list()
  counter=1
  currentTime=0
  while(currentTime <= nrOfOnlineInstances){
    performance = obtainVerificationPerformanceAtTimeStep(onlineLearnerData, currentTime)
    performanceList[[counter]] = performance
    timepointList[[counter]]= currentTime
    currentTime = currentTime + timestepSize
    counter=counter+1
  }
  
  if(currentTime<nrOfOnlineInstances+timestepSize){ #Some remainder instances
    performance = obtainVerificationPerformanceAtTimeStep(onlineLearnerData, nrOfOnlineInstances)
    performanceList[[counter]] = performance
    timepointList[[counter]] = nrOfOnlineInstances
  }
  
  resList = list()
  resList[["timepointList"]] = timepointList
  resList[["performanceOverviewList"]] = performanceList
  return(resList)
  
}

#Returns the performance overview from the models at the specified timestep on the set of verification instances (specified in onlineLearnerData$onlineScenario)
obtainVerificationPerformanceAtTimeStep = function(onlineLearnerData, timestep){
  models = getModelsAtTimestep(onlineLearnerData,timestep)
  selectionOverview = getSelectionOverviewForInstanceSet(models, onlineLearnerData$onlineScenario$aslibScenario, onlineLearnerData$onlineScenario$verificationSet, onlineLearnerData$onlineScenario$consideredFeatures)
  performanceMeasureOverview = obtainPerformanceMeasureOverview(onlineLearnerData$onlineScenario$aslibScenario, selectionOverview)
  return(performanceMeasureOverview)
}



#Returns the models of all algorithms at the specified timestep in a named list (keyed by the algorithm names)
#A timestep value of 100 means that the models will be returned as they were before making the prediction for instance 101
#Note: not the exact same models will be returned. During the online phase only the mlr learner tasks are stored, not thee xact models. So the models used are generated by using the same task, but due to stochasticity differences might arise
getModelsAtTimestep = function(onlineLearnerData, timestep){
  modelsList = list()
  for(algorithm in onlineLearnerData$onlineScenario$consideredAlgorithms){
    regrTask = getTaskOfAlgorithmAtTimestep(onlineLearnerData, algorithm, timestep)
    modelsList[[algorithm]] = train(onlineLearnerData$mlrLearner,regrTask) 
  }
  return(modelsList)
}



#Returns the learner task of the specified algorithm at the specified timestep
#A timestep value of 100 means that the task will be returned as they were before making the prediction for instance 101
getTaskOfAlgorithmAtTimestep = function(onlineLearnerData, algorithm, timestep){
  instancesHandledSoFar = getAmountOfInstancesHandledByAlgorithmAtTimestep(onlineLearnerData, algorithm, timestep) 
  trainingInstances = length(onlineLearnerData$onlineScenario$trainingSet)
  maxTotalObservations = trainingInstances+instancesHandledSoFar#Sum of training instances (shared by all algs) and the online instances of this specific algo. Is the total amount of datapoints at the specified timestep
  
  
  taskList = onlineLearnerData$regressionTaskMatrixOverTime[[algorithm]]
  
  
  currentTask = taskList[[1]] #First task
  
  if(length(taskList) > 1){
    for(i in 2:length(taskList)){
      potentialNextTask = taskList[[i]]
      if(potentialNextTask$task.desc$size <= maxTotalObservations){
        currentTask = potentialNextTask 
      }
    }
  }
  
 
  return(currentTask)
}


#Returns the amount of instances that have been handled by the specified algorithm at the specified time
#If 0 or less: returns 0
getAmountOfInstancesHandledByAlgorithmAtTimestep = function(onlineLearnerData, algorithm, timestep){
  if(timestep <=0){
    return(0) 
  }
  onlineInstanceTimeMap = onlineLearnerData$instanceTimeMap
  instancesHandledByAlg = onlineLearnerData$onlineScenario$onlineInstanceOverview[[algorithm]]

  amountOfHandledInstances = 0
  for(i in 1:timestep){
    instance = onlineInstanceTimeMap[[i]]
    if(instance %in% instancesHandledByAlg){
       amountOfHandledInstances = amountOfHandledInstances+1
    }
  }
  return(amountOfHandledInstances)
}

#Calculates the performance of the current selection models and adds it to the timestep performanceInfo
#If a timestep for the same amount of instances handled already exists, it is overwritten
addTimestepPerformanceToOnlineLearnerData = function(onlineLearnerData){
  currentTimestep = length(onlineLearnerData$instanceTimeMap) #Amount of handled online instances
  
  if(length(onlineLearnerData$performanceInfo$timestep$availableTimesteps) > 0){
    lastTimestepWithPerformanceInfo = tail(onlineLearnerData$performanceInfo$timestep$availableTimesteps,1)
  }
  else{
    lastTimestepWithPerformanceInfo = -1 #no timestep performance info yet
  }
  #Only if the new timestep is actually different from the last one is it added. 
  #(if not: the performance info will be overwritten)
  if(currentTimestep != lastTimestepWithPerformanceInfo){
    onlineLearnerData$performanceInfo$timestep$availableTimesteps = c(onlineLearnerData$performanceInfo$timestep$availableTimesteps, currentTimestep)
  }
  
  
  models = getCurrentModelList(onlineLearnerData)
  selectionOverview = getSelectionOverviewForInstanceSet(models, onlineLearnerData$onlineScenario$aslibScenario, onlineLearnerData$onlineScenario$verificationSet, onlineLearnerData$onlineScenario$consideredFeatures)
  
  performanceOverviewDataTable = createPerformanceOverviewForSelectionOverview(selectionOverview, onlineLearnerData$onlineScenario)
    
  positionOfNewTimepoint = length(onlineLearnerData$performanceInfo$timestep$availableTimesteps)
  timepointInfo = performanceOverviewDataTable 
  onlineLearnerData$performanceInfo$timestep$selectionModelQualityList[[positionOfNewTimepoint]] = timepointInfo
  
  return(onlineLearnerData)
}

#Adds observed performance and some additional performance columns to the selectionOverview
#returns the result as a datatable
createPerformanceOverviewForSelectionOverview = function(selectionOverview, onlineScenario){
  instNames = selectionOverview$instanceId
  
  selectionOverviewWithPerf = extendSelectionOverviewWithPerformance(selectionOverview, onlineScenario$aslibScenario)
  
  
  sbsPerf = getPerformances(onlineScenario$singleBest, instNames, onlineScenario$aslibScenario) #single best solver
  swsPerf = getPerformances(onlineScenario$singleWorst, instNames, onlineScenario$aslibScenario) #single best solver #single worst solver
  vbsPerf = getVirtualBestSolverPerformance(onlineScenario$aslibScenario, instNames, onlineScenario$consideredAlgorithms) #virtual best solver
  vwsPerf = getVirtualWorstSolverPerformance(onlineScenario$aslibScenario, instNames, onlineScenario$consideredAlgorithms) #virtual best solver #virtual worst solver
  
  #merging all into one data.table
  vbsSwsMerged = merge(sbsPerf, swsPerf, by="instance_id", suffixes = c(".sbs", ".sws"))
  vbSwsVbsMerged = merge(vbsSwsMerged, vbsPerf, by = "instance_id")
  allExtraMerged = merge(vbSwsVbsMerged, vwsPerf, by = "instance_id")
  
  allMerged = merge(selectionOverviewWithPerf, allExtraMerged, by = "instance_id")
  
  colnames(allMerged) = c("instance_id", "selectedAlgorithm", "observedPerformance", "singleBest", "singleWorst", "virtualBest", "virtualWorst")
  
  return(allMerged)
}


#Normalises all the performance values in the selectionModelQuality with respect to the the single best solver and vbs on the verification set (the same set on which these performance values were originally defined)
normaliseTimestepPerformanceVerification = function(onlineLearnerData){
  verificationSbs = getSingleBestPerformanceOverview(llamaScenario = onlineLearnerData$onlineScenario$llamaScenarioVerificationOnly) 
  verificationVbs = getVbsPerformanceOverview(llamaScenario = onlineLearnerData$onlineScenario$llamaScenarioVerificationOnly) 

  for(i in 1:length(onlineLearnerData$performanceInfo$timestep$selectionModelQualityList)){
    thisPerformance = onlineLearnerData$performanceInfo$timestep$selectionModelQualityList[[i]]
    onlineLearnerData$performanceInfo$timestep$selectionModelQualityList[[i]]= normalisePerformanceDataFrame(thisPerformance, sbsPerformance = verificationSbs, vbsPerformance = verificationVbs)
  }
  
  return(onlineLearnerData)
  
  
}

#Refactor: put in better place and make ther methods use it too
#Returns the normalised performance based on observed, vbs and singleBest performances, with perf vbs = 1 and sbs = 0
getNormalisedPerformance = function(observedPerf, singleBestPerf, vbsPerf){
  #To get dimensions right
  result= observedPerf
  divisor = observedPerf
  
  #Calculating divisor: sbsPerf-vbsPerf
  for(name in names(observedPerf)){
    divisor[[name]] = singleBestPerf[[name]]-vbsPerf[[name]]
  }
  #divisor = singleBestPerf-vbsPerf
  
  #Calculating value: (observedPerf-vbsPerf)/divisor
  for(name in names(observedPerf)){
    result[[name]] = 1- (observedPerf[[name]]-vbsPerf[[name]])/divisor[[name]]
  }
  #value = (observedPerf-vbsPerf)/divisor
  return(result)
  
}



#Adds an additional column to the selectionOverview, containing the performance of the selected algorithm
extendSelectionOverviewWithPerformance = function(selectionOverview, aslibScenario){
  selectionOverview = data.table(selectionOverview)
  performancesList = c()
  for(instance in selectionOverview$instanceId){
    selectedAlg = selectionOverview[instanceId == instance,selectedAlgorithm]
    performance = subset(aslibScenario$algo.runs, algorithm == selectedAlg & instance_id == instance)$performance
    performancesList = c(performancesList, performance)
  }
  resultDataTable = cbind(selectionOverview, performancesList)
  colnames(resultDataTable) = c("instance_id", "selectedAlgorithm", "performance")
  return(resultDataTable)
}
  
  
  
  

