#Creates an S3 object that contains a couple of performance measures as well as the selectionOverview and aslibScenario which have been used to calculate the scores
#Contains the following performance measures:
# meanPar10Score
# meanPar1Score
# nrOfSuccesses
# proportionOfSuccesses
# misclassificationPenalties
obtainPerformanceMeasureOverview = function(llamaScenario, selectionOverview){
  llamaSelectionOverview = createLlamaPredictionsDataFrame(selectionOverview)  
  
  meanPar10Score = obtainMeanParScoreLlama(llamaScenario, llamaSelectionOverview, factor=10)
  meanPar1Score = obtainMeanParScoreLlama(llamaScenario, llamaSelectionOverview, factor=1)
  nrOfSuccesses = obtainNrOfSuccessesLlama(llamaScenario, llamaSelectionOverview)
  proportionOfSuccesses = nrOfSuccesses/nrow(llamaSelectionOverview)
  averageMisclassificationPenalty = obtainAverageMisclassificationPenaltyLlama(llamaScenario, llamaSelectionOverview)
  
  performanceMeasureOverview = makeS3Obj("performanceMeasureOverview",meanPar10Score = meanPar10Score, meanPar1Score = meanPar1Score, nrOfSuccesses = nrOfSuccesses, proportionOfSuccesses = proportionOfSuccesses, averageMisclassificationPenalty = averageMisclassificationPenalty)

  return(performanceMeasureOverview)
  
}
#Creates a predictions dataframe that can be interpreted by LLAMA based on a selection overview dataframe (which has one column with instances and another column with the selected algorithms)
createLlamaPredictionsDataFrame = function(selectionOverview){
  predictions = data.frame("instance_id" = selectionOverview$instanceId, "algorithm" = selectionOverview$selectedAlgorithm, "score" = rep(-1,length(selectionOverview$instanceId)), "iteration" = rep(1, length(selectionOverview$instanceId))) 
  return(predictions)
}

#Returns the mean par score as calculated by the LLAMA package for the instance-algorithm combinations in selectionOverview
obtainMeanParScoreLlama = function(llamaScenario, llamaPredictions, factor) {
  llamaInputList = list(predictions=llamaPredictions)
  attr(llamaInputList, "addCosts") = TRUE
  attr(llamaInputList, "hasPredictions") = TRUE
  return(mean(parscores(llamaScenario, llamaInputList, factor=factor))) 
}

#Returns the amount of successfully solved instances in selectionOverview, according to the aslibScenario
obtainNrOfSuccessesLlama = function(llamaScenario, llamaPredictions){
  llamaInputList = list(predictions=llamaPredictions, factor=factor)
  attr(llamaInputList, "addCosts") = TRUE
  attr(llamaInputList, "hasPredictions") = TRUE
  successesList = successes(llamaScenario, llamaInputList)
  nrOfSuccesses = length(successesList[successesList==TRUE])
  return(nrOfSuccesses)
  
}

#Returns the average misclassification penalty for the instance-algorithm combinations in selectionOverview, according to the data in aslibScenario
obtainAverageMisclassificationPenaltyLlama = function(llamaScenario, llamaPredictions){
  llamaInputList = list(predictions=llamaPredictions, factor=factor)
  attr(llamaInputList, "addCosts") = TRUE
  attr(llamaInputList, "hasPredictions") = TRUE
  averageMisclassificationPenalty = mean(misclassificationPenalties(llamaScenario, llamaInputList))
  return(averageMisclassificationPenalty)
}

#Creates a performance overview for the virtual best solver of the specified scenario 
getVbsPerformanceOverview = function(llamaScenario){
  return(getPerformanceOverviewForFunction(llamaScenario, vbs, "vbs", aslibScenario)) 
}

#Creates a performance overview for the single best solver of the specified scenario for the considered instances
getSingleBestPerformanceOverview = function(llamaScenario){
  return(getPerformanceOverviewForFunction(llamaScenario, singleBest, "singleBest", aslibScenario))
}

#Creates a performance overview for the single best solver of the specified scenario for the considered instances
getPerformanceOverviewForFunction = function(llamaScenario, functionSpecification, functionName, aslibScenario){
  averageMisclassificationPenalty = mean(misclassificationPenalties(llamaScenario, functionSpecification, addCosts=TRUE))
  successOverview = successes(llamaScenario, functionSpecification, addCosts = TRUE)
  nrOfSuccesses = length(subset(successOverview, successOverview == TRUE))
  proportionOfSuccesses = nrOfSuccesses/length(successOverview)
  meanPar1Score = mean(parscores(llamaScenario, functionSpecification, addCosts=TRUE, factor=1)) 
  meanPar10Score = mean(parscores(llamaScenario, functionSpecification, addCosts=TRUE, factor=10)) 
  
  performanceMeasureOverview = makeS3Obj("performanceMeasureOverview",meanPar10Score = meanPar10Score, meanPar1Score = meanPar1Score, nrOfSuccesses = nrOfSuccesses, proportionOfSuccesses = proportionOfSuccesses, averageMisclassificationPenalty = averageMisclassificationPenalty)

  return(performanceMeasureOverview)
}



