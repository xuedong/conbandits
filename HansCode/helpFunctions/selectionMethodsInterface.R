

#batchtools
simulateGreedy = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                          batchSize, minNrOfTrainingInst = 5){

  onlineLearnerData = instance  
  onlineScenario = instance$onlineScenario
  
  onlineLearnerData$nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining
  onlineLearnerData$keepOldRegressionTasks = keepOldRegressionTasks
  onlineLearnerData$doTimeDependentVerification=doTimeDependentVerification
  onlineLearnerData$doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation

  #onlineLearnerData=instance 
  
  #Checks if there is sufficient training data available.
  #If not: generate it by solving the first few online instances with algorithms for which more training data is needed
  onlineLearnerData = generateMissingTrainingData(onlineLearnerData, minNrOfTrainingInst)
  
  
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)

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
  

  #Retrain all models in order to obtain all online info 
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
  }
  

  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from postProcessingHelpers
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from postProcessingHelpers
  
  #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
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
simulateLcb = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                       batchSize, lambda,minNrOfTrainingInst = 5){
  
  
  onlineLearnerData = instance  
  onlineScenario = instance$onlineScenario
  
  onlineLearnerData$nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining
  onlineLearnerData$keepOldRegressionTasks = keepOldRegressionTasks
  onlineLearnerData$doTimeDependentVerification=doTimeDependentVerification
  onlineLearnerData$doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation
  
  #Checks if there is sufficient training data available.
  #If not: generate it by solving the first few online instances with algorithms for which more training data is needed
  onlineLearnerData = generateMissingTrainingData(onlineLearnerData, minNrOfTrainingInst)
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  newBatch = list()
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchLcb(newBatch, onlineLearnerData,lambda)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
  }
  
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchLcb(newBatch, onlineLearnerData,lambda)  
    newBatch = list()
  }
  
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
  }
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  
  #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
  
  return(onlineLearnerData)
}




#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the lcb method and updates the online data and regression models
handleInstanceBatchLcb = function(newInstanceIdsList, onlineLearnerData,lambda){
  selectedAlgorithmOverview = selectAlgorithmsForBatchLcb(newInstanceIdsList, onlineLearnerData,lambda)
  newSelectionData = transformSelectedAlgorithmOverviewToSelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}


#Iterates over all runtime instances in batches of specified size'
#Greedily selects the predicted best algorithm for each instance
#Returns an onlineLearnerdata object with all information about the online selection process
#Note: an aslib scenario is assumed, meaning that for all instances in onlineInstanceList performance data is available in the aslibScenario contained in onlineLearnerData
# simulateGreedy = function(dynamic, batchSize ){
#   onlineLearnerData=dynamic #batchJobs needs arg name dynamic, this simplifies the conversion and removing it later if so desired
#   
#   onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#   newBatch = list()
#   for(instanceId in onlineLearnerData$onlineScenario$runtimeSet){
#     newBatch = c(unlist(newBatch), instanceId)
#     if(length(newBatch) >= batchSize){
#       onlineLearnerData = handleInstanceBatchGreedy(newBatch, onlineLearnerData)  
#       onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#       newBatch = list()
#     }
#   }
#   
#   
#   #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
#   if(length(newBatch)>0){
#     onlineLearnerData = handleInstanceBatchGreedy(newBatch, onlineLearnerData)  
#     newBatch = list()
#   }
#   
#   #Retrain all models in order to obtain all online info 
#   for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
#     onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
#   }
#   
#   onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#   onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from postProcessingHelpers
#   onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from postProcessingHelpers
#   
#   #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
#   onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
#   
#   
#   return(onlineLearnerData)
# }



#Iterates over all runtime instances
#Selects the predicted best algorithm for each instance according to the models trained only on the training data (so no online learning)
#Retuns an onlineLearnerdata object with all information about the online selection process
simulateGreedyNoLearning = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                                    batchSize, minNrOfTrainingInst = 5){
  
  onlineLearnerData = instance  
  onlineScenario = instance$onlineScenario
  
  onlineLearnerData$nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining
  onlineLearnerData$keepOldRegressionTasks = keepOldRegressionTasks
  onlineLearnerData$doTimeDependentVerification=doTimeDependentVerification
  onlineLearnerData$doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation
  
  #Checks if there is sufficient training data available.
  #If not: generate it by solving the first few online instances with algorithms for which more training data is needed
  onlineLearnerData = generateMissingTrainingData(onlineLearnerData, minNrOfTrainingInst)
  
  
  
  #Ensure that retraining never happens by retraining a model after an infinite amount of steps
  #This overwrites any value that the user might have specified
  originalValueOfOnlineLearnerDataNrOfStepsWithoutRetraining = onlineLearnerData$nrOfStepsWithoutRetraining
  onlineLearnerData$nrOfStepsWithoutRetraining = Inf
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  newBatch = list()
  
  
  for(instanceId in instancesToHandle){
    newBatch = c(unlist(newBatch), instanceId)
    if(length(newBatch) >= batchSize){
      onlineLearnerData = handleInstanceBatchGreedyNL(newBatch, onlineLearnerData)  
      onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
      newBatch = list()
    }
  }
  
  
  #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
  if(length(newBatch)>0){
    onlineLearnerData = handleInstanceBatchGreedyNL(newBatch, onlineLearnerData)  
    newBatch = list()
  }
  
  
 
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  
  #No Learning never retrains its models
  
  
  #Remove the models because they take too much space. Can be retrained later by using the last regressionTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
  #Restore the original value of nrOfStepsWithoutRetraining
  onlineLearnerData$nrOfStepsWithoutRetraining = originalValueOfOnlineLearnerDataNrOfStepsWithoutRetraining
  
  return(onlineLearnerData)
}

#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the 'pick predicted best' algorithm policy and updates nothing as it's No learning
handleInstanceBatchGreedyNL =  function(newInstanceIdsList, onlineLearnerData){
  selectedAlgorithmOverview = selectBestAlgorithmBatch(newInstanceIdsList, onlineLearnerData)
  newSelectionData = createEmptySelectionDataOverview(selectedAlgorithmOverview)
  onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview,newSelectionData )
  return(onlineLearnerData)
}

#Always select the best algorithm, but use feedback of all algorithms (also those not selected)
simulateGreedyFullInfo = function(data,instance,job,nrOfStepsWithoutRetraining, keepOldRegressionTasks, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation,
                                  batchSize, minNrOfTrainingInst = 5){
  
  onlineLearnerData = instance  
  onlineScenario = instance$onlineScenario
  
  onlineLearnerData$nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining
  onlineLearnerData$keepOldRegressionTasks = keepOldRegressionTasks
  onlineLearnerData$doTimeDependentVerification=doTimeDependentVerification
  onlineLearnerData$doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation
  
  #Checks if there is sufficient training data available.
  #If not: generate it by solving the first few online instances with algorithms for which more training data is needed
  onlineLearnerData = generateMissingTrainingData(onlineLearnerData, minNrOfTrainingInst)
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  

  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
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
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  
  
  #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
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
                                 batchSize, epsilon, minNrOfTrainingInst = 5 ){
  onlineLearnerData = instance  
  onlineScenario = instance$onlineScenario
  
  onlineLearnerData$nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining
  onlineLearnerData$keepOldRegressionTasks = keepOldRegressionTasks
  onlineLearnerData$doTimeDependentVerification=doTimeDependentVerification
  onlineLearnerData$doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation
  
  #Checks if there is sufficient training data available.
  #If not: generate it by solving the first few online instances with algorithms for which more training data is needed
  onlineLearnerData = generateMissingTrainingData(onlineLearnerData, minNrOfTrainingInst)
  
  #All instance ids that are in the runtimeset that have not been used to generate additional training data can be used
  instancesToHandle = setdiff(onlineLearnerData$onlineScenario$runtimeSet, names(onlineLearnerData$selectionOverview))
  
  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
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
  
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
  }

  onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
  onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
  
  
  #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
  onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
  
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


#WARNING: untested
#Iterates over all runtime instances in batches of specified size'
#Selects the predicted worst algorithm for each instance 
#Returns an onlineLearnerdata object with all information about the online selection process
#Note: an aslib scenario is assumed, meaning that for all instances in onlineInstanceList performance data is available in the aslibScenario contained in onlineLearnerData
# simulateSelectWorst = function(dynamic, batchSize ){
#   onlineLearnerData=dynamic #batchJobs needs arg name dynamic, this simplifies the conversion and removing it later if so desired
#   
#   onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#   newBatch = list()
#   for(instanceId in onlineLearnerData$onlineScenario$runtimeSet){
#     newBatch = c(unlist(newBatch), instanceId)
#     if(length(newBatch) >= batchSize){
#       onlineLearnerData = handleInstanceBatchSelectWorst(newBatch, onlineLearnerData)  
#       newBatch = list()
#     }
#   }
#   
#   
#   #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
#   if(length(newBatch)>0){
#     onlineLearnerData = handleInstanceBatchSelectWorst(newBatch, onlineLearnerData)  
#     onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#     newBatch = list()
#   }
#   
#     for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
#       onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
#     }
#   
#   
#   onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#   onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
#   onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
#   
#   
#   #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
#   onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
#   
#   return(onlineLearnerData)
# }
# 
# #Handles the specified list of online instances for an artificial aslib scenario
# #selects the algorithms to solve them with according to the 'pick predicted worst' algorithm policy and updates the online data and regression models
# handleInstanceBatchSelectWorst =  function(newInstanceIdsList, onlineLearnerData){
#   selectedAlgorithmOverview = selectWorstAlgorithmBatch(newInstanceIdsList, onlineLearnerData)
#   onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview)
#   return(onlineLearnerData)
# }

#WARNING: untested
#Iterates over all runtime instances in batches of specified size'
#Selects a random algorithm for each instance 
#Returns an onlineLearnerdata object with all information about the online selection process
#Note: an aslib scenario is assumed, meaning that for all instances in onlineInstanceList performance data is available in the aslibScenario contained in onlineLearnerData
# simulateSelectRandom = function(dynamic, batchSize ){
#   onlineLearnerData=dynamic #batchJobs needs arg name dynamic, this simplifies the conversion and removing it later if so desired
#   
#   onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#   newBatch = list()
#   for(instanceId in onlineLearnerData$onlineScenario$runtimeSet){
#     newBatch = c(unlist(newBatch), instanceId)
#     if(length(newBatch) >= batchSize){
#       onlineLearnerData = handleInstanceBatchSelectRandom(newBatch, onlineLearnerData)  
#       onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#       newBatch = list()
#     }
#   }
#   
#   #Handle remaining instances in case batch size doesn't divide the amount of instances exactly
#   if(length(newBatch)>0){
#     onlineLearnerData = handleInstanceBatchSelectRandom(newBatch, onlineLearnerData)  
#     newBatch = list()
#   }
#   
#     for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
#       onlineLearnerData = retrainModel(onlineLearnerData, algorithmName)
#     }
#  
#   onlineLearnerData = handleTimepointPerformance(onlineLearnerData)
#   onlineLearnerData = addRuntimePerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
#   onlineLearnerData = addVerificationPerformanceInfoToOnlineLearnerData(onlineLearnerData) #from onlineLearnerDataHelpers
#   
#   #Remove the models because they take too much space. Can be retrained later by using the last modelTasks
#   onlineLearnerData = removeModelsFromOnlineLearnerData(onlineLearnerData)
#   
#   return(onlineLearnerData)
# }


#Handles the specified list of online instances for an artificial aslib scenario
#selects the algorithms to solve them with according to the 'pick random algorithm' policy and updates the online data and regression models
# handleInstanceBatchSelectRandom =  function(newInstanceIdsList, onlineLearnerData){
#   selectedAlgorithmOverview = selectRandomAlgorithmBatch(newInstanceIdsList, onlineLearnerData)
#   onlineLearnerData = addBatchOfTimePointsToOnlineLearnerData(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview)
#   return(onlineLearnerData)
# }

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


#Performs a normal offline experiment: traings on a fraction pOnlineAsTraining of the online data
simulateOffline = function(data,instance,job,pOnlineAsTraining, doTimeDependentVerification, doTimeDependentRegressionModelEvaluation){
  onlineLearnerData = instance  
  onlineScenario = instance$onlineScenario

  onlineLearnerData$keepOldRegressionTasks = keepOldRegressionTasks
  onlineLearnerData$doTimeDependentVerification=doTimeDependentVerification
  onlineLearnerData$doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation
  
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
  
  #Ensure that retraining never happens by retraining a model after an infinite amount of steps
  #This overwrites any value that the user might have specified
  originalValueOfOnlineLearnerDataNrOfStepsWithoutRetraining = onlineLearnerData$nrOfStepsWithoutRetraining
  onlineLearnerData$nrOfStepsWithoutRetraining = Inf
  
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
  
  #Restore the original value of nrOfStepsWithoutRetraining
  onlineLearnerData$nrOfStepsWithoutRetraining = originalValueOfOnlineLearnerDataNrOfStepsWithoutRetraining
  
  return(onlineLearnerData)
  
    
}

  
  

