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
