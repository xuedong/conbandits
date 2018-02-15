selectMlrLearner = function(mlrLearnerName, predictionType = "response"){
  if(mlrLearnerName == "regr.randomForest"){
    mlrLearner = getRandomForestLearner(predictionType)
  }
  else if(mlrLearnerName == "regr.ranger"){
    mlrLearner = getRangerLearner(predictionType)
  }
  else if(mlrLearnerName == "regr.lm"){
    mlrLearner = getLinearRegressionLearner(predictionType)
  }
  else if(mlrLearnerName == "regr.randomForest.jackKnife"){
    mlrLearner = getRandomForestLearnerJackknife(predictionType)
  }
#  else if(mlrLearnerName == "LinUCB"){
#    mlrLearner = "LinUCB"
#  }
  else{
    stop(paste("Invalid mlrlearner method name ", mlrLearnerName, "specified; mlr learner cannot be loaded"))
  }
  return(mlrLearner)
}

doStandardPreProcessing = function(onlineScenario, nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification,
                                   doTimeDependentRegressionModelEvaluation, mlrLearnerName, predictionType = "response",  minNrOfTrainingInst){

  mlrLearner = selectMlrLearner(mlrLearnerName, predictionType)
  onlineLearnerData = initialiseOnlineLearnerData(onlineScenario = onlineScenario, mlrLearner = mlrLearner, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining,
                                                  keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification, 
                                                  doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation)

  #Checks if there is sufficient training data available.
  #If not: generate it by solving the first few online instances with algorithms for which more training data is needed
  onlineLearnerData = generateMissingTrainingData(onlineLearnerData, minNrOfTrainingInst)
  
  #generate performanceoverview for first timepoint
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  
  return(onlineLearnerData)
}


#Obtain performanceInfo
#remove regression models and regression tasks
doStandardPostProcessing = function(onlineLearnerData){
  #Retrain all models in order to obtain all online info 
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
  }
  
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from postProcessingHelpers
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from postProcessingHelpers
  
  #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
  if(! onlineLearnerData$keepOldRegressionTasks){
    onlineLearnerData = removeTasksFromOnlineLearnerData(onlineLearnerData)
  }
  
  return(onlineLearnerData)
}



#batchtools
simulateGreedy = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                          batchSize, mlrLearnerName, minNrOfTrainingInst = 5){

  onlineScenario = instance  

  onlineLearnerData = doStandardPreProcessing(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = minNrOfTrainingInst)
  

  newBatch = list()
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchGreedy(newBatch, onlineLearnerData)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
  }

  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchGreedy(newBatch, onlineLearnerData)  
    newBatch = list()
  }
  

  onlineLearnerData = doStandardPostProcessing(onlineLearnerData)
      
  return(onlineLearnerData)
}



#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the 'pick predicted best' algorithm policy and updates the online data and regression models
handleInstanceBatchGreedy =  function(newInstanceIdsList, onlineLearnerData){
  selectedAlgorithmOverview = selectBestAlgorithmBatch(newInstanceIdsList, onlineLearnerData)
  newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}

#Iterates over all runtime instances in batches of specified size'
#Selects the predicted best algorithm for each instance according to lcb
#Returns an onlineLearnerdata object with all information about the online selection process
#Note: an aslib scenario is assumed, meaning that for all instances in onlineInstanceList performance data is available in the aslibScenario contained in onlineLearnerData
simulateUcb = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                       batchSize, mlrLearnerName, lambda,minNrOfTrainingInst = 5){
  
  
  onlineScenario = instance  
  
  onlineLearnerData = doStandardPreProcessing(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, predictionType = "se", minNrOfTrainingInst = minNrOfTrainingInst)
  
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  newBatch = list()
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchUcb(newBatch, onlineLearnerData,lambda)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
  }
  
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchUcb(newBatch, onlineLearnerData,lambda)  
    newBatch = list()
  }
  
  onlineLearnerData = doStandardPostProcessing(onlineLearnerData)
  
  
  
  return(onlineLearnerData)
}




#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the lcb method and updates the online data and regression models
handleInstanceBatchUcb = function(newInstanceIdsList, onlineLearnerData,lambda){
  selectedAlgorithmOverview = selectAlgorithmsForBatchUcb(newInstanceIdsList, onlineLearnerData,lambda)
  newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}

#Always select the best algorithm, but use feedback of all algorithms (also those not selected)
simulateGreedyFullInfo = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                                  batchSize, mlrLearnerName, minNrOfTrainingInst = 5){
  
  onlineScenario = instance  
  
  onlineLearnerData = doStandardPreProcessing(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = minNrOfTrainingInst)
  
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  newBatch = list()
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchGreedyFullInfo(newBatch, onlineLearnerData)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
  }
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchGreedyFullInfo(newBatch, onlineLearnerData)  
    newBatch = list()
  }
 
   
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
  }
  
  onlineLearnerData = doStandardPostProcessing(onlineLearnerData)
  
  
  
  return(onlineLearnerData)
}

#Handles the specified batch of online instances by applying the greedy select best criterion
#selects the algorithms to solve them with according to the pick best algorithm policy and updates the online data and regression models
handleInstanceBatchGreedyFullInfo = function(newInstanceIdsList, onlineLearnerData){
  selectedAlgorithmOverview = selectBestAlgorithmBatch(newInstanceIdsList, onlineLearnerData)
  newSelectionData = obtainFullInfoSelectionDataOverview(selectedAlgorithmOverview, onlineLearnerData$onlineScenario$consideredAlgorithms)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
  
  
}


#Iterates over all runtime instances in batches of specified size'
#Selects the predicted best algorithm for each instance according to epsilon greedy
#Returns an onlineLearnerdata object with all information about the online selection process
#Note: an aslib scenario is assumed, meaning that for all instances in onlineInstanceList performance data is available in the aslibScenario contained in onlineLearnerData
simulateEpsilonGreedy = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                                 batchSize, mlrLearnerName, epsilon, minNrOfTrainingInst = 5 ){
  onlineScenario = instance  
  
  onlineLearnerData = doStandardPreProcessing(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = minNrOfTrainingInst)
  
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  newBatch = list()
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchEpsilonGreedy(newBatch, onlineLearnerData,epsilon)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
  }
  
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchEpsilonGreedy(newBatch, onlineLearnerData,epsilon)  
    newBatch = list()
  }
  
  onlineLearnerData = doStandardPostProcessing(onlineLearnerData)
  
  
  return(onlineLearnerData)
}




#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the epsilon greedy method and updates the online data and regression models
handleInstanceBatchEpsilonGreedy = function(newInstanceIdsList, onlineLearnerData,epsilon){
  selectedAlgorithmOverview = selectAlgorithmsForBatchEpsilonGreedy(newInstanceIdsList, onlineLearnerData,epsilon)
  newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}



#Iterates over all runtime instances in batches of specified size'
#Selects the predicted best algorithm for each instance according to epsilon first, 
#meaning it selects a random algorithm for the first epsilon instances, and then selects greedily
#Returns an onlineLearnerdata object with all information about the online selection process
#Note: an aslib scenario is assumed, meaning that for all instances in onlineInstanceList performance data is available in the aslibScenario contained in onlineLearnerData
simulateEpsilonFirst = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                                 batchSize, mlrLearnerName, epsilon, minNrOfTrainingInst = 5 ){
  onlineScenario = instance  
  
  onlineLearnerData = doStandardPreProcessing(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, 
                                              keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification,
                                              doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation, 
                                              mlrLearnerName = mlrLearnerName, minNrOfTrainingInst = minNrOfTrainingInst)
  
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  nrOfRandomInstances = round(length(instancesToHandle)*epsilon)
  
  if(nrOfRandomInstances > 0){
    randomInstances = instancesToHandle[1:nrOfRandomInstances]
  }else{
    randomInstances = list()
  }
  
  #If instances remain: make them the greedy instances
  if(length(randomInstances) < length(instancesToHandle)){
    greedyInstances = instancesToHandle[(nrOfRandomInstances+1):length(instancesToHandle)]
  }else{
    greedyInstances = list()
  }
  
  #How it works:
  #For the random instances, standard epsilon-greedy is run with epsilon value == 1 (pure random)
  #For the next instances, standard epsilon greedy is run with epsilon value == 0 (pure greedy)
  
  #Phase 1: random selections
  epsilon = 1
  
  newBatch = list()
  for(instanceId in randomInstances){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      
      onlineLearnerData = handleInstanceBatchEpsilonGreedy(newBatch, onlineLearnerData,epsilon )  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
    
  }
  
  #Handle remaining instances in case batch size doesn't divide the amount of random instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchEpsilonGreedy(newBatch, onlineLearnerData,epsilon)  
    newBatch = list()
  }
  
  
  #Phase 2: greedy selections
  epsilon = 0
  
  newBatch = list()
  for(instanceId in greedyInstances){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      
      onlineLearnerData = handleInstanceBatchEpsilonGreedy(newBatch, onlineLearnerData,epsilon )  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
    
  }
  
  #Handle remaining instances in case batch size doesn't divide the amount of random instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchEpsilonGreedy(newBatch, onlineLearnerData,epsilon)  
    newBatch = list()
  }
  
  
  
  onlineLearnerData = doStandardPostProcessing(onlineLearnerData)
  
  
  return(onlineLearnerData)
}


#Performs a normal offline experiment: traings on a fraction pOnlineAsTraining of the online data
simulateOffline = function(data,instance,job,pOnlineAsTraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation, mlrLearnerName){
  onlineScenario = instance  
  
  mlrLearner = selectMlrLearner(mlrLearnerName)
  #Ensure that retraining never happens by retraining a model after an infinite amount of steps (nrOfStepsWithouytRetraining value)
    onlineLearnerData = initialiseOnlineLearnerData(onlineScenario = onlineScenario, mlrLearner = mlrLearner, nrOfStepsWithoutRetraining = Inf,
                                                  keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification, 
                                                  doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation)
  

  availableInstances = onlineLearnerData$onlineScenario$runtimeSet
  amountToTrainOn = round(length(availableInstances)*pOnlineAsTraining)
  
  if(amountToTrainOn>0){
    trainingInst = availableInstances[1:amountToTrainOn]
  }else{
    trainingInst = c()
  }
  
  onlineLearnerData$onlineScenario$trainingSet = union(onlineScenario$trainingSet,trainingInst)
  
  for(algorithm in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData = retrainModel(onlineLearnerData, algorithmName = algorithm)
  }
  
  
  newBatch = list()
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  

  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, trainingInst)
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  newBatch = list()
  
  
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
  }
  
  
  #Handle the instances
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchGreedyNL(newBatch, onlineLearnerData)  
    newBatch = list()
  }
  
  
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  if(pOnlineAsTraining < 1){ #if not there simply is no online performance, as no online instances were handled
    onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  }
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  
  #No Learning never retrains its models
  
  
  #Remove the models because they take too much space. Can be retrained later by using the last regressionTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
  if(! onlineLearnerData$keepOldRegressionTasks){
    onlineLearnerData = removeTasksFromOnlineLearnerData(onlineLearnerData)
  }

  return(onlineLearnerData)
}


handleInstanceBatchGreedyNL = function(newInstanceIdsList, onlineLearnerData){
  selectedAlgorithmOverview = selectBestAlgorithmBatch(newInstanceIdsList, onlineLearnerData)
  newSelectionData = createEmptySelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}




handleTimepointPerformance = function(onlineLearnerData){
  #Order is important for consequent updating of the list of timepoints! See addTimestepRegressionModelQualityInformationToOnlineLearnerData
  if(onlineLearnerData$doTimeDependentVerification){
    onlineLearnerData = addTimestepPerformanceToOnlineLearnerData(onlineLearnerData)
  }
  if(onlineLearnerData$doTimeDependentRegressionModelEvaluation){
    
    onlineLearnerData = addTimestepRegressionModelQualityInformationToOnlineLearnerData(onlineLearnerData)
  }
  return(onlineLearnerData)
}



#Takes a prediction overview (A list indexed by instance names with for each instance one algorithm name)
#returns the corresponding selectionDataOverview(A list indexed by instance names with for each instance a list of algorithms
#note: in this case each list of algorithms consists of exactly one algorithm
transformSelectedAlgorithmOverviewToSelectionDataOverview = function(selectedAlgorithmOverview){
   selectionDataOverview = selectedAlgorithmOverview
   for(inst in names(selectionDataOverview)){
      selectionDataOverview[[inst]] = list(selectionDataOverview[[inst]]) 
     
   }
   return(selectionDataOverview)
}
  
 
#Creates a newSelectionData object that contains for each instance specifed in instanceIds the empty list 
#used by greedyNL
#Really stupidly coded (doesn't need selectedAlgorihtmOverview, only instanceIds) but this was faster to code
createEmptySelectionDataOverview = function(selectedAlgorithmOverview){
  selectionDataOverview = selectedAlgorithmOverview
  for(inst in names(selectionDataOverview)){
    selectionDataOverview[[inst]] = list()
  }
  return(selectionDataOverview)
}

obtainFullInfoSelectionDataOverview = function(selectedAlgorithmOverview, allAlgorithms){
   
  selectionDataOverview = selectedAlgorithmOverview
  for(inst in names(selectionDataOverview)){
    selectionDataOverview[[inst]] = allAlgorithms
  }
  return(selectionDataOverview)
}



#Generates additional trainin data by processing the first few online instances
#Generates new training data until each algorithm has 'requiredNrOfTrainingInstPerAlg' training inst
#Does not produce any new data if there already is sufficient for each algorihtm
generateMissingTrainingData = function(onlineLearnerData, requiredNrOfTrainingInstPerAlg){
  nrOfTrainingInst = length(onlineLearnerData$onlineScenario$trainingSet)
  listOfAlgorithmsToSelect = c()
  
  
  if(nrOfTrainingInst < requiredNrOfTrainingInstPerAlg){
    nrOfRequiredDatapoints = requiredNrOfTrainingInstPerAlg - nrOfTrainingInst
    algList = onlineLearnerData$onlineScenario$consideredAlgorithms
    selectionOrder = rep(algList, nrOfRequiredDatapoints) #round robin seleciton order
    
    selectionOverview = list()
    counter = 1
    newInstanceIdsList = c()
    for(alg in selectionOrder){
      instance = onlineLearnerData$onlineScenario$runtimeSet[[counter]]
      selectionOverview[[instance]] = alg
      newInstanceIdsList = c(newInstanceIdsList, instance)
      counter = counter+1
    }
   
    newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectionOverview)
    #Ensure that the models are trained by setting the amount of instances required for retraining to 0
    nrOfStepsWithoutRetraining = onlineLearnerData$nrOfStepsWithoutRetraining
    onlineLearnerData$nrOfStepsWithoutRetraining = 0
    onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectionOverview,newSelectionData )
    #Put back to original
    onlineLearnerData$nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining
    
  }
  
  return(onlineLearnerData)
    
    
    
}




  
  
