initialiseAdditionalInfoMap = function(onlineScenario){
  additionalInfoMap = list()
  for(algorithmName in onlineScenario$consideredAlgorithms){
    additionalInfoMap[[algorithmName]] = list()
  }
  onlineScenario$additionalInfoMap = additionalInfoMap
  return(onlineScenario)
}

#Adds as many extra information points to the additionalInfoMap as specified in amountOfAdditionalInfoAlgorithms for every instance in selectedAlgortihmOverview
#Obviously the already selected algorithm's info will never be added
addDatapointsToAdditionalInfoMap = function(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview, amountOfAdditionalInfoAlgorithms){
  for(instance in newInstanceIdsList){
    selectedAlgo = selectedAlgorithmOverview[[instance]]
    allOtherAlgorithms = setdiff(onlineLearnerData$onlineScenario$consideredAlgorithms, names(selectedAlgo))
    #Obtain the algorithms for which additional info must be added randomly
    selectedAlgorithms = sample(allOtherAlgorithms, size=amountOfAdditionalInfoAlgorithms)

    for(algorithmName in selectedAlgorithms){
      onlineLearnerData$onlineScenario = addOnlineInstanceToAdditionalInfoMap(instance, algorithmName, onlineLearnerData$onlineScenario)
      onlineLearnerData$nrOfStepsSinceLastTrainingList[[algorithmName]] = onlineLearnerData$nrOfStepsSinceLastTrainingList[[algorithmName]] +1
    }
    
  }
  return(onlineLearnerData)
}

#Adds the specified instance to the additionalInfoMap of the specified algorihtm
addOnlineInstanceToAdditionalInfoMap = function(instanceId, algorithmName, onlineScenario){
  onlineScenario$additionalInfoMap[[algorithmName]] = c(unlist(onlineScenario$additionalInfoMap[[algorithmName]]), instanceId)
  return(onlineScenario)
}