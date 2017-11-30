#Methods related to creating and updating an onlineLearnerData object


#Creates an initial onlineLearnerData object. It is an S3 object consisting of:
# onlineScenario: an onlineScenario object 
#	mlrLearner: the mlr learner method used to create new models
#	instanceTimeMap: a mapping of timepoints to instances denoting which instancene was handled at what time
#	regressionTaskMatrixOverTime: an overview of the current regression tasks - one for each algorithm- and, if storeOldModels is TRUE, the previous regression tasks
#	regressionModelList: an overview of the current regression models –one for each algorithm-
#	nrOfStepsWithoutRetraining: the amount of instances an algorithm solves before its model is retrained
#	nrOfStepsSinceLastTrainingList: a list with one entry for each algorithm containing for each algorithm the amount of steps since its model was last retrained
#	keepOldRegressionTasks: a Boolean denoting whether or not to store outdated regressionTasks
# d○doTimeDependentVerification: a boolean denoting whether or not to measure the performance of the models on the verification set after each batch 
# doTimeDependentRegressionModelEvaluation: a boolean denoting whether or not to measure the performance of the individual regression models (1/algorithm) after each batch
# selectionOverview: a list with for each handled online instance the algorithm that was used to solve it
initialiseOnlineLearnerData = function(onlineScenario, mlrLearner, nrOfStepsWithoutRetraining = 0, keepOldRegressionTasks=TRUE,doTimeDependentVerification=TRUE, doTimeDependentRegressionModelEvaluation = TRUE){
  #Create a regression model for each algorithm
  #At initialisation stage the tasks will only contain offline instances
  
  if(length(onlineScenario$trainingSet)!=  0){
    regressionTaskList = createListOfRegressionTasks(onlineScenario)
    
    #Train a model for each algorithm
    regressionModelList = createListOfRegressionModels(regressionTaskList, mlrLearner)
  }
  else{
    regressionTaskList = list()
    regressionModelList = list()
  }

  #Initialise the list containing the instances handled at each time. Initially this list is empty
  instanceTimeMap = list() 
  
  #Initialise the list of for each algorithm the amount of steps since it was trained.
  #Since all algorithms are trained in this method, all algorithms receive value 0
  nrOfStepsSinceLastTrainingList = list()
  
  for(algorithmName in onlineScenario$consideredAlgorithms){
    nrOfStepsSinceLastTrainingList[[algorithmName]] = 0
  }
  
  #Initialise the overview of regression tasks and regression models with the initial model
  regressionTaskMatrixOverTime = list()
  for(algorithmName in onlineScenario$consideredAlgorithms){
    regressionTaskMatrixOverTime[[algorithmName]] = list()
  }
  

  selectionOverview = list()
  

  #Create S3 object
  onlineLearnerData = makeS3Obj("onlineLearnerData", onlineScenario = onlineScenario, mlrLearner = mlrLearner, instanceTimeMap = instanceTimeMap, regressionTaskMatrixOverTime = regressionTaskMatrixOverTime, regressionModelList = regressionModelList, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, nrOfStepsSinceLastTrainingList = nrOfStepsSinceLastTrainingList, selectionOverview=selectionOverview, keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification, doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation)
  

  return(onlineLearnerData) 
}


initialiseEmptyOnlineLearnerData = function(onlineScenario, mlrLearner, nrOfStepsWithoutRetraining = 0, keepOldRegressionTasks=TRUE,doTimeDependentVerification=TRUE, doTimeDependentRegressionModelEvaluation = TRUE){
  #Create a regression model for each algorithm
  #At initialisation stage the tasks will only contain offline instances
  regressionTaskList = NULL
  
  #Train a model for each algorithm
  regressionModelList = NULL
  
  
  #Initialise the list containing the instances handled at each time. Initially this list is empty
  instanceTimeMap = list() 
  
  #Initialise the list of for each algorithm the amount of steps since it was trained.
  #Since all algorithms are trained in this method, all algorithms receive value 0
  nrOfStepsSinceLastTrainingList = list()
  
  for(algorithmName in onlineScenario$consideredAlgorithms){
    nrOfStepsSinceLastTrainingList[[algorithmName]] = 0
  }
  
  #Initialise the overview of regression tasks and regression models with the initial model
  regressionTaskMatrixOverTime = list()
  for(algorithmName in onlineScenario$consideredAlgorithms){
    regressionTaskMatrixOverTime[[algorithmName]] = list()
  }
  
  
  selectionOverview = list()
  
  
  #Create S3 object
  onlineLearnerData = makeS3Obj("onlineLearnerData", onlineScenario = onlineScenario, mlrLearner = mlrLearner, instanceTimeMap = instanceTimeMap, regressionTaskMatrixOverTime = regressionTaskMatrixOverTime, regressionModelList = regressionModelList, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, nrOfStepsSinceLastTrainingList = nrOfStepsSinceLastTrainingList, selectionOverview=selectionOverview, keepOldRegressionTasks = keepOldRegressionTasks, doTimeDependentVerification = doTimeDependentVerification, doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation)
  
  
  return(onlineLearnerData) 
}




#Adds a batch of timepoints to the onlineLearnerDataObject
#This entails adding the instance handled at the new timepoint 
#updating the amount of steps the selected algorithm has been without retraining
#Updating the selectionOverview
#and potentially relearning the model of the selected algorithm
#Note: selectedAlgorithmOvereview contains for each instance in 'newInstanceIdsList' exactly one algorithm (the one used to solve it)
#Note: newSelectionData contains for each instance in 'newInstanceIdsList' a list of algorithms (can be 0) describing for which algorithms the performance data on the instance can be used to update the regression models
addBatchOfTimePointsToOnlineLearnerData = function(onlineLearnerData, newInstanceIdsList, selectedAlgorithmOverview, newSelectionData){
  
  
  #Adding the new instances to the onlineScenario
  for(instanceId in newInstanceIdsList){
    for(algorithm in newSelectionData[[instanceId]]){
      onlineLearnerData$onlineScenario = addOnlineInstanceToAlgorithm(instanceId, algorithm, onlineLearnerData$onlineScenario)  
    }
  }
  
  #selectionoverview update
  for(instanceId in newInstanceIdsList){
    onlineLearnerData = addNewInstanceTimepoint(onlineLearnerData, instanceId)  
    onlineLearnerData = addNewSelectionToSelectionOverview(onlineLearnerData, selectedAlgorithmOverview[[instanceId]], instanceId)
  }
  
  
  #Update amount of times without retraining by iterating over all new datapoints that have been added
  for(instanceId in newInstanceIdsList){
    for(algorithm in newSelectionData[[instanceId]]){
      onlineLearnerData$nrOfStepsSinceLastTrainingList[[algorithm]] = onlineLearnerData$nrOfStepsSinceLastTrainingList[[algorithm]]+1  
   }
  }
  
  #[1] "idastar.symmullt.transmul"

  
  #Check if a selection models must be retrained
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    if(onlineLearnerData$nrOfStepsSinceLastTrainingList[[algorithmName]] > onlineLearnerData$nrOfStepsWithoutRetraining){
      onlineLearnerData$nrOfStepsSinceLastTrainingList[[algorithmName]] = 0
      onlineLearnerData = retrainModel(onlineLearnerData, algorithmName = algorithmName)
    }
  }
  return(onlineLearnerData)
}

#Add a new selection, consisting of an instance and the algorithm that was used to solve it
#Currently each instance should be solved by exactly one algorithm
addNewSelectionToSelectionOverview = function(onlineLearnerData, selectedAlgorithm, instance){
  onlineLearnerData$selectionOverview[[instance]] = selectedAlgorithm
  return(onlineLearnerData)
}

#Retrains the model of the specified algorithm using all available runtimedata and the training data
#If the keepOldRegressionTasks parameter of onlineLearnerData==TRUE, the old Tasks are saved. The old Models are still thrown away because they take up a lot of memory, but they can be retrained by training the stores tasks with the learner
retrainModel = function(onlineLearnerData, algorithmName){
  #retrain the model
  newRegressionTask = createMlrRegressionTask(algorithmName, onlineLearnerData$onlineScenario)
  newModel = train(onlineLearnerData$mlrLearner,newRegressionTask) 
  #Check if the old model needs to be stored or replaced
  if(onlineLearnerData$keepOldRegressionTasks){
    lastPositionRegressionTask = length(onlineLearnerData$regressionTaskMatrixOverTime[[algorithmName]])
    onlineLearnerData$regressionTaskMatrixOverTime[[algorithmName]][[lastPositionRegressionTask+1]] = newRegressionTask
  }
  else{
    onlineLearnerData$regressionTaskMatrixOverTime[[algorithmName]][[1]] = newRegressionTask
  }
  onlineLearnerData$regressionModelList[[algorithmName]] = newModel
  
  return(onlineLearnerData)
  
}
#Add the specified instance to the instanceTimeMap of onlineLearnerData
#It will be appended to the list, signifying an addition for the latest timepoint
addNewInstanceTimepoint = function(onlineLearnerData, newInstanceId){
  onlineLearnerData$instanceTimeMap = c(onlineLearnerData$instanceTimeMap, newInstanceId) 
  return(onlineLearnerData)
}



#Returns a list of models, one for each algorithm, that would be used to predict the algorithm to select for the next instance
#Not useful after the update in which old models are never stored anymore, only their tasks
getCurrentModelList = function(onlineLearnerData){
  return(onlineLearnerData$regressionModelList)
}

#Returns a list of models, one for each algorithm, that would be used to predict the algorithm to select for the next isntance
getCurrentTaskList = function(onlineLearnerData){
  mostRecentTasks = list()
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    #Long code to get the last element in the list of the considered algorithm
    mostRecentTasks[[algorithmName]] = onlineLearnerData$regressionTaskMatrixOverTime[[algorithmName]][[length(onlineLearnerData$regressionTaskMatrixOverTime[[algorithmName]])]]
  }
  return(mostRecentTasks)
}

#Returns an overview dataframe of which instance was solved by which algorithm.
#The instances are ordered by the order in which they were solved (= the order they're stored in onlineLearnerData$instanceTimeMap)
obtainOrderedSelectionOverview = function(onlineLearnerData){
  orderedSelectionOverview =makeDataFrame(nrow = length(onlineLearnerData$instanceTimeMap), ncol = 2, 
                                   col.types = c("character", "character") ,
                                   col.names = c("instanceId", "selectedAlgorithm"))

  counter=1
  for(instanceId in onlineLearnerData$instanceTimeMap){
    selectedAlgorithm = onlineLearnerData$selectionOverview[[instanceId]]
    orderedSelectionOverview[counter,] = list(instanceId, selectedAlgorithm)
    counter=counter+1
  }
  
  return(orderedSelectionOverview)
}


removeModelsFromOnlineLearnerData = function(onlineLearnerData){
  for(algorithmName in onlineLearnerData$onlineScenario$consideredAlgorithms){
    onlineLearnerData$regressionModelList[[algorithmName]] = NULL
  }
  return(onlineLearnerData)
}




