#Adds a new field to the onlineLearnerData containing the performance observed during the online phase of the simulation
addRuntimePerformanceInfoToOnlineLearnerData = function(onlineLearnerData){
  #onlineLearnerData$performanceInfo$runtime$observedPerformance = extractPerformanceMeasuresFromOnlineLearnerData(onlineLearnerData)
  selectionOverview = obtainOrderedSelectionOverview(onlineLearnerData)
  
  onlineLearnerData$performanceInfo$runtime = createPerformanceOverviewForSelectionOverview(selectionOverview, onlineLearnerData$onlineScenario)
  
  #VBS and singleBest performance
  #vbsPerformanceOverview =  getVbsPerformanceOverview(llamaScenario = onlineLearnerData$onlineScenario$llamaScenarioOnlineOnly) 
  #onlineLearnerData$performanceInfo$runtime$vbsPerformance = vbsPerformanceOverview
  
  #singleBestPerformanceOverview = getSingleBestPerformanceOverview(llamaScenario = onlineLearnerData$onlineScenario$llamaScenarioOnlineOnly) 
  #onlineLearnerData$performanceInfo$runtime$singleBestPerformance = singleBestPerformanceOverview
  
  return(onlineLearnerData)
}

#Adds a new field to the onlineLearnerData containing the performance observed during the verification phase of the simulation
addVerificationPerformanceInfoToOnlineLearnerData = function(onlineLearnerData){
  modelList = getCurrentModelList(onlineLearnerData)
  
  selectionOverview = getSelectionOverviewForInstanceSet(modelList, onlineLearnerData$onlineScenario$aslibScenario, onlineLearnerData$onlineScenario$verificationSet, onlineLearnerData$onlineScenario$consideredFeatures)
  onlineLearnerData$performanceInfo$verification = createPerformanceOverviewForSelectionOverview(selectionOverview, onlineLearnerData$onlineScenario)
  
  
  #performanceMeasureOverview = obtainPerformanceMeasureOverview(onlineLearnerData$onlineScenario$llamaScenario, selectionOverview)
  ##can't pass entire scenario?
  
  #onlineLearnerData$performanceInfo$verification$selectionOverview = selectionOverview
  #onlineLearnerData$performanceInfo$verification$observedPerformance = performanceMeasureOverview
  
  #VBS and singleBest performance
  #vbsPerformanceOverview =  getVbsPerformanceOverview(llamaScenario = onlineLearnerData$onlineScenario$llamaScenarioVerificationOnly) 
  #onlineLearnerData$performanceInfo$verification$vbsPerformance = vbsPerformanceOverview
  
  #singleBestPerformanceOverview = getSingleBestPerformanceOverview(llamaScenario = onlineLearnerData$onlineScenario$llamaScenarioVerificationOnly) 
  #onlineLearnerData$performanceInfo$verification$singleBestPerformance = singleBestPerformanceOverview
  
  return(onlineLearnerData)
}

#Adds the verification performance when the selection mapping itself (including exploration)
#is considered. Contrast to addVerificationPerformance, where only the underlying
#regression models are considered
#the passed selectionFunction can have up to 1 argument, which can be specified with
#selectionFunctionArg1. If none is specified, it is ignored.
addSelectionMappingVerificationPerformance = function(onlineLearnerData, selectionFunction, selectionFunctionArg1 = NULL){
  verificationInstances = onlineLearnerData$onlineScenario$verificationSet
  if(is.null(selectionFunctionArg1)){
    batchSelections = selectionFunction(verificationInstances,onlineLearnerData)
  }
  else{
    batchSelections = selectionFunction(verificationInstances,onlineLearnerData, selectionFunctionArg1)
    
  }
  selectionOverview = transformBatchSelectionFunctionOutputToSelectionOverview(batchSelections)
  perfOverview = createPerformanceOverviewForSelectionOverview(selectionOverview, onlineLearnerData$onlineScenario)
  onlineLearnerData$performanceInfo$selectionMappingVerification = perfOverview
  return(onlineLearnerData)
}

#Transform the output of a function like handleInstanceBatchGreedy to the same
#output as function getSelectionOverviewForInstanceSet (a data frame)
transformBatchSelectionFunctionOutputToSelectionOverview = function(batchSelections){
  selectionOverview =makeDataFrame(nrow = length(names(batchSelections) ), ncol = 2, 
                                   col.types = c("character", "character") ,
                                   col.names = c("instanceId", "selectedAlgorithm"))
  selectionOverview[,1] = names(batchSelections) 
  
  for(instanceId in names(batchSelections)){
    selectionOverview[[selectionOverview$instanceId == instanceId,2]] = batchSelections[[instanceId]]
  }
  return(selectionOverview)
  
}

#Returns the selection overview of the specified models on the instance set
#Does this by making the predictions using the models
getSelectionOverviewForInstanceSet = function(models, aslibScenario, instanceList, consideredFeatures){
  predictionOverviews = list()
  newInstanceFeaturesOverviewWithInstId = getFeatureValuesForInstList(instanceList, consideredFeatures, aslibScenario)
  newInstanceFeaturesOverview = subset(newInstanceFeaturesOverviewWithInstId, TRUE, select = names(newInstanceFeaturesOverviewWithInstId)[2:length(names(newInstanceFeaturesOverviewWithInstId))])
  
  for(algorithmName in names(models)){
    predictionBatch = predict(models[[algorithmName]],newdata=  newInstanceFeaturesOverview)
    predictionOverviews[[algorithmName]] = getPredictionResponse(predictionBatch)
    names(predictionOverviews[[algorithmName]]) = newInstanceFeaturesOverviewWithInstId[,"instance_id"]
  }
  
  
  selectionOverview =makeDataFrame(nrow = length(instanceList), ncol = 2, 
                                   col.types = c("character", "character") ,
                                   col.names = c("instanceId", "selectedAlgorithm"))
  selectionOverview[,1] = instanceList 
  
  
  for(instanceId in instanceList){
    predictedRuntimes = list()
    for(algorithmName in names(models)){
      predictedRuntimes[[algorithmName]] = predictionOverviews[[algorithmName]][[instanceId]]
    }
    bestAlg = selectAlgorithmWithBestPredictedPerformance(predictedRuntimes)
    selectionOverview[[selectionOverview$instanceId == instanceId,2]] = bestAlg
  }
  
  
  
  return(selectionOverview)
  
}


#Returns a performanceMeasureOverview object containing some performance measures for the specified job
#Doubled code in onlineLearnerData's 
#extractPerformanceMeasuresFromOnlineLearnerData = function(onlineLearnerData){
#  selectionOverview = obtainOrderedSelectionOverview(onlineLearnerData)
#  performanceOverview = obtainPerformanceMeasureOverview(onlineLearnerData$onlineScenario$llamaScenario, selectionOverview)
#  return(performanceOverview)
#}


#Returns the normalised version of the performance denoted in observedPerformance
#sbsPerformance and vbsPerformance must have identical structure to observedPerformance
normalisePerformanceDataFrame = function(observedPerformance, sbsPerformance, vbsPerformance){
  resPerformance = observedPerformance #To get correct dimensions

  for(i in 1:length(observedPerformance)){
    if(names(observedPerformance)[[i]] != "timepoint"){
      divisor = sbsPerformance[[i]]-vbsPerformance[[i]]
      value = (observedPerformance[[i]]-vbsPerformance[[i]])/divisor
      resPerformance[[i]] = value
    }
    
  }
  return(resPerformance)
}
