#Returns the best algorithm according to the most recent models for each algorithm
selectBestAlgorithmBatch = function(newInstanceIdsList, onlineLearnerData){
  selectionOverview = list()
  
  batchOfPredictedRuntimes = getBatchOfPredictedRuntimes(newInstanceIdsList, onlineLearnerData)
  for(instanceId in newInstanceIdsList){
    bestAlgorithm = selectAlgorithmWithBestPredictedRuntime(batchOfPredictedRuntimes[instanceId,])
    selectionOverview[[instanceId]] = bestAlgorithm
  }
  
  #Selects the best algorithm for each instance
  return(selectionOverview)
}


#Returns the predicted runtimes for a batch of instances
#WARNING: some code duplication with getBatchOfLcbValues in algSelectionLcb.R
getBatchOfPredictedRuntimes = function(newInstanceIdsList, onlineLearnerData){
  
  
  
  newInstanceFeaturesOverviewWithInstId = getFeatureValuesForInstList(newInstanceIdsList, onlineLearnerData$onlineScenario$consideredFeatures, onlineLearnerData$onlineScenario$aslibScenario)
  newInstanceFeaturesOverview = subset(newInstanceFeaturesOverviewWithInstId, TRUE, select = names(newInstanceFeaturesOverviewWithInstId)[2:length(names(newInstanceFeaturesOverviewWithInstId))])
  
    
  predictedRuntimeOverview = list()
  #Obtain a list with the most recent models from the onlineLearnerData
  
  currentModelsList = getCurrentModelList(onlineLearnerData) 
  
  
  #Obtain the predictions for each algorithm  
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    predictionBatch = predict(currentModelsList[[algorithmName]],newdata=  newInstanceFeaturesOverview)
    
    meansOfBatch = getPredictionResponse(predictionBatch)
    names(meansOfBatch) = newInstanceFeaturesOverviewWithInstId[,"instance_id"]
    
    #append column: col of runtimes of this alg
    predictedRuntimeOverview = cbind(predictedRuntimeOverview, meansOfBatch)
    
  }
  colnames(predictedRuntimeOverview) = onlineLearnerData$onlineScenario$consideredAlgorithms
  
  #Returns correct predictions for the instances according to the
  return (predictedRuntimeOverview)
  
}

#Returns the algorithm with highest predicted runtime given the runttimes in 'predictedPerformancesList'
#The elements in 'predictedPerformancesList' must be indexed by the algorithm names
#Returns the best algorithm according to the most recent models for each algorithm
selectWorstAlgorithmBatch = function(newInstanceIdsList, onlineLearnerData){
  selectionOverview = list()
  
  batchOfPredictedRuntimes = getBatchOfPredictedRuntimes(newInstanceIdsList, onlineLearnerData)
  for(instanceId in newInstanceIdsList){
    bestAlgorithm = selectAlgorithmWithWorstPredictedRuntime(batchOfPredictedRuntimes[instanceId,])
    selectionOverview[[instanceId]] = bestAlgorithm
  }
  
  #Selects the best algorithm for each instance
  return(selectionOverview)
}


#select a random algorithm for each instance in newInstanceIdsList
#The elements in 'predictedPerformancesList' must be indexed by the algorithm names
selectRandomAlgorithmBatch = function(newInstanceIdsList, onlineLearnerData){
  selectionOverview = list()
  
  for(instanceId in newInstanceIdsList){
    bestAlgorithm = selectRandomAlgorithm(instanceId, onlineLearnerData)
    selectionOverview[[instanceId]] = bestAlgorithm
  }
  
  #Selects the best algorithm for each instance
  return(selectionOverview)
}

#Selects a random algorithm
#Doesn't really need the newInstanceId argument
selectRandomAlgorithm = function(newInstanceId, onlineLearnerData){
  algorithmList = onlineLearnerData$onlineScenario$consideredAlgorithms
  randomAlg = sample(algorithmList,1)
  return( as.character(randomAlg))
}

#Returns the algorithm with highest predicted runtime given the runtimes in 'predictedPerformancesList'
#The elements in 'predictedPerformancesList' must be indexed by the algorithm names
selectAlgorithmWithWorstPredictedRuntime = function(predictedPerformancesList){
  maxIndex = which.max(predictedPerformancesList)
  return(maxIndex)
}

selectAlgorithmWithBestPredictedRuntime = function(predictedPerformancesList){
  minIndex = which.min(predictedPerformancesList)
  return(names(predictedPerformancesList)[[minIndex]])
}

#Returns data which can be fed to an mlr model to make predictions for the specified instances
getMlrPredictionCompatibleFeatureData = function(newInstanceIdsList, onlineLearnerData){
  newInstanceFeaturesOverviewWithInstId = getFeatureValuesForInstList(newInstanceIdsList, onlineLearnerData$onlineScenario$consideredFeatures, onlineLearnerData$onlineScenario$aslibScenario)
  newInstanceFeaturesOverview = subset(newInstanceFeaturesOverviewWithInstId, TRUE, select = names(newInstanceFeaturesOverviewWithInstId)[2:length(names(newInstanceFeaturesOverviewWithInstId))])
  return(newInstanceFeaturesOverview)
}



