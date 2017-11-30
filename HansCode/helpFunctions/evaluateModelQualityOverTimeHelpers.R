#Assumption: this method is always called after addTimestepPerformanceToOnlineLearnerData if that method is called 
addTimestepRegressionModelQualityInformationToOnlineLearnerData = function(onlineLearnerData, timepoint){
  currentTimestep = length(onlineLearnerData$instanceTimeMap) #Amount of handled online instances
  
  #Otherwise: timestepslist has already been extended
  #Quite dirty code
  if(!onlineLearnerData$doTimeDependentVerification){
    onlineLearnerData$performanceInfo$timestep$availableTimesteps = c(onlineLearnerData$performanceInfo$timestep$availableTimesteps, currentTimestep)
  }
  
  models = getCurrentModelList(onlineLearnerData)
  
  
  positionOfNewTimepoint = length(onlineLearnerData$performanceInfo$timestep$availableTimesteps)
  newInstanceIdsList = onlineLearnerData$onlineScenario$verificationSet
  
  #copies from getBatchOfPredictedRuntimes in algSelectionSharedMethods.R
  newInstanceFeaturesOverviewWithInstId = getFeatureValuesForInstList(newInstanceIdsList, onlineLearnerData$onlineScenario$consideredFeatures, onlineLearnerData$onlineScenario$aslibScenario)
  newInstanceFeaturesOverview = subset(newInstanceFeaturesOverviewWithInstId, TRUE, select = names(newInstanceFeaturesOverviewWithInstId)[2:length(names(newInstanceFeaturesOverviewWithInstId))])
  predictionBatches = list()
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    predictionBatches[[algorithmName]] = predict(models[[algorithmName]],newdata=  newInstanceFeaturesOverview)
    rownames(predictionBatches[[algorithmName]]$data) = newInstanceFeaturesOverviewWithInstId[,1]
  }
  
  onlineLearnerData$performanceInfo$timestep$regressionModelQualityList[[positionOfNewTimepoint]] = predictionBatches
  return(onlineLearnerData)
  
}

getAvgDistanceBetweenPredictedValuesAndActuaLValuesOverTimeOverview = function(onlineLearnerData){
  
  aslibScenario = onlineLearnerData$onlineScenario$aslibScenario
  listOfAvgDistanceDataFrames = list()
  for(algorithmId in onlineLearnerData$onlineScenario$consideredAlgorithms){
    
    avgDistanceDf = makeDataFrame(nrow = 1, ncol = 2, 
                                  col.types = c("numeric", "numeric"), 
                                  col.names = c("timepoint","avgDifference"))
    for(rowNr in 1:length(onlineLearnerData$performanceInfo$timestep$availableTimesteps)){
      predictionOverview = onlineLearnerData$performanceInfo$timestep$regressionModelQualityList[[rowNr]]
      perfOverview = getDistancesBetweenPredictedValuesAndActualValues(predictionOverview, aslibScenario, algorithmId)
      avgDistance = mean(perfOverview$difference)
      avgDistanceDf[rowNr,] = list(onlineLearnerData$performanceInfo$timestep$availableTimesteps[[rowNr]], avgDistance)
    }
    listOfAvgDistanceDataFrames[[algorithmId]] = avgDistanceDf
  }
  
  return(listOfAvgDistanceDataFrames)
}

#Returns a three columned dataframe, 1: prediction, 2: actual value, 3: difference
getDistancesBetweenPredictedValuesAndActualValues = function(predictionOverview, aslibScenario, algorithmName){
  resPredictionOverview = makeDataFrame(nrow = 1, ncol = 4, 
                                 col.types = c("numeric", "numeric", "numeric", "numeric"), 
                                 col.names = c("instanceId","predictedValue", "actualValue", "difference"))
  
  counter=1
  for(instanceId in rownames(predictionOverview[[algorithmName]]$data)){
    actualValue = getRuntimes(algorithmId = algorithmName, instanceList = instanceId, scenario = aslibScenario)$runtime
    predictedValue=  predictionOverview[[algorithmName]]$data[instanceId,]$response
    resPredictionOverview[counter,] = list(instanceId, predictedValue, actualValue, abs(predictedValue-actualValue) )
    counter = counter+1
  }
  
  return(resPredictionOverview)
  
}





