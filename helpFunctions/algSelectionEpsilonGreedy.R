

#Makes algorithm selections for a batch of instances, specified in newInstanceIdsList
selectAlgorithmsForBatchEpsilonGreedy = function(newInstanceIdsList, onlineLearnerData,epsilon){
  batchOfPredictions = getBatchOfPredictedPerformances(newInstanceIdsList, onlineLearnerData)
  selectionOverview = list()
  
  #Select the best algorihtm or a random algorithm for each algorithm, depending on randomness and the specified epsilon value
  for(instanceId in newInstanceIdsList){
    consideredValues = batchOfPredictions[instanceId,]
    randomNr = runif(1)
    if(randomNr < (1 - epsilon)){ #Pick predicted best
      bestAlgorithm = selectAlgorithmWithBestPredictedPerformance(consideredValues)
      selectionOverview[[instanceId]] = bestAlgorithm
    }
    else{
      randomAlg = selectRandomAlgorithm(instanceId, onlineLearnerData)  
      selectionOverview[[instanceId]] = randomAlg
    }
  } 
  return(selectionOverview)
}