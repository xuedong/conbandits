#Returns the algorithm with highest ucb value in ucbValueList (assumed lcbValueList is indexed by algorithmname)
selectAlgorithmWithHighestUcbValue = function(ucbValueList){
  maxIndex = which.max(ucbValueList)
  chosenAlg = names(ucbValueList)[[maxIndex]]
  return(chosenAlg)
}



#Makes algorithm selections for a batch of instances, specified in newInstanceIdsList
selectAlgorithmsForBatchUcb = function(newInstanceIdsList, onlineLearnerData,lambda){
  batchOfUcbValues = getBatchOfUcbValues(newInstanceIdsList, onlineLearnerData, lambda)
  
  selectionOverview = list()
  
  #Find the algorithm with lowest lcb value for each instance
  for(instanceId in newInstanceIdsList){
    consideredValues = batchOfUcbValues[instanceId,]
    bestAlgorithm = selectAlgorithmWithHighestUcbValue(consideredValues)
    selectionOverview[[instanceId]] = bestAlgorithm
  } 
  return(selectionOverview)
  
  
}

#Returns a list with elements identifiable by algorithmnames containing the ucb value of the algorithm for the specified list of instances
#All algorithms in the consideredAlgorithms field of onlineLearnerData$onlineScenario are considered
#WARNING: some code duplication with getBatchOfPredictedRuntimes in algSelectionSharedMethods.R
getBatchOfUcbValues = function(newInstanceIdsList, onlineLearnerData,lambda){
  ucbValueOverview = list()
  newInstanceFeaturesOverviewWithInstId = getFeatureValuesForInstList(newInstanceIdsList, onlineLearnerData$onlineScenario$consideredFeatures, onlineLearnerData$onlineScenario$aslibScenario)
  newInstanceFeaturesOverview = subset(newInstanceFeaturesOverviewWithInstId, TRUE, select = names(newInstanceFeaturesOverviewWithInstId)[2:length(names(newInstanceFeaturesOverviewWithInstId))])
  
  currentTimestep = length(onlineLearnerData$instanceTimeMap)+1
  
  #Obtain a list with the most recent models from the onlineLearnerData
  currentModelsList = getCurrentModelList(onlineLearnerData) 
  

  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    predictionBatch = predict(currentModelsList[[algorithmName]],newdata=  newInstanceFeaturesOverview)
    
    meansOfBatch = getPredictionResponse(predictionBatch)
    sesOfBatch = getPredictionSE(predictionBatch)
    names(meansOfBatch) = newInstanceFeaturesOverviewWithInstId[,"instance_id"]
    names(sesOfBatch) = newInstanceFeaturesOverviewWithInstId[,"instance_id"]
    
    ucbValues = calcUcbValuesOfBatch(meansOfBatch, sesOfBatch, lambda, currentTimestep) 
    
    ucbValueOverview = cbind(ucbValueOverview, ucbValues)
    #append column: col of lcbValues of this alg
  }
  colnames(ucbValueOverview) = onlineLearnerData$onlineScenario$consideredAlgorithms
  return (ucbValueOverview)
}



#Calculates the lcbValues of a batch based on a list of means and a list of standard deviations
#meanList and seList must be identifiable by instance_id and must contain values for exactly the same instance_id's
#The value = predicted average + lambda*predicted standard deviation
calcUcbValuesOfBatch = function(meanList, seList, lambda, timestep){
  ucbValueList = list()
  for(instanceId in names(meanList)){
    mean = meanList[[instanceId]]
    se = seList[[instanceId]]
    ucbValueList[[instanceId]] = mean+lambda*se*sqrt(log(timestep) )
  }
  return(ucbValueList)
}





#Based on Auer at al's Fnite-time Analysis of the Multiarmed Bandit Problem => UCB1 policy
#Returns a list with elements identifiable by algorithmnames containing the LCB1 value of the algorithm for the specified list of instances
#All algorithms in the consideredAlgorithms field of onlineLearnerData$onlineScenario are considered
#WARNING: some code duplication with getBatchOfPredictedRuntimes in algSelectionSharedMethods.R
# getBatchOfProperLcb1Values = function(newInstanceIdsList, onlineLearnerData){
#   lcbValueOverview = list()
#   newInstanceFeaturesOverviewWithInstId = getFeatureValuesForInstList(newInstanceIdsList, onlineLearnerData$onlineScenario$consideredFeatures, onlineLearnerData$onlineScenario$aslibScenario)
#   newInstanceFeaturesOverview = subset(newInstanceFeaturesOverviewWithInstId, TRUE, select = names(newInstanceFeaturesOverviewWithInstId)[2:length(names(newInstanceFeaturesOverviewWithInstId))])
#   
#   #Obtain a list with the most recent models from the onlineLearnerData
#   currentModelsList = getCurrentModelList(onlineLearnerData) 
#   
#   
#   for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
#     
#     
#     predictionBatch = predict(currentModelsList[[algorithmName]],newdata=  newInstanceFeaturesOverview)
#     
#     meansOfBatch = getPredictionResponse(predictionBatch)
#     names(meansOfBatch) = newInstanceFeaturesOverviewWithInstId[,"instance_id"]
# 
#     lcbValues = calcProperLcb1ValuesOfBatch(meansOfBatch, sesOfBatch, lambda)
#     
#     lcbValueOverview = cbind(lcbValueOverview, lcbValues)
#     #append column: col of lcbValues of this alg
#   }
#   colnames(lcbValueOverview) = onlineLearnerData$onlineScenario$consideredAlgorithms
#   return (lcbValueOverview)
# }
# 
# #Calculates the lcbValues of a batch based on a list of means and a list of standard deviations
# #meanList and seList must be identifiable by instance_id and must contain values for exactly the same instance_id's
# #The value = predicted average - lambda*predicted standard deviation
# calcProperLcb1ValuesOfBatch = function(meanList, nrOfInstancesHandledSoFar, nrOfInstancesHandledSoFarByThisAlg, lambda){
#   lcbValueList = list()
#   lcb1Correction = sqrt((2*log(nrOfInstancesHandledSoFar))/nrOfInstancesHandledSoFarByThisAlg)
#     
#   for(instanceId in names(meanList)){
#     mean = meanList[[instanceId]]
#     lcbValueList[[instanceId]] = mean-lambda*se 
#   }
#   return(lcbValueList)
# }



