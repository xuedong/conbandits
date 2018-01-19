#Functions used to create mlr regression tasks and models


#Creates a regression task for each algorithm in the 'consideredAlgorithms' field of onlineScenario and adds them to a list.
#Each task is identifiable by the algorithm for which it is created
#Each task considers all offline instances (shared by all algorithms) and for each algorithm its online instances (not necessarily sharded by all algorithms)
createListOfRegressionTasks = function(onlineScenario){
  regressionTasks = list()
  for(algorithmName in onlineScenario$consideredAlgorithms){
    newTask = createMlrRegressionTask(algorithmName, onlineScenario)
    regressionTasks[[algorithmName]] = newTask
  }
  return(regressionTasks) 
}

#Creates a regression model using 'mlrLearner' for each task in 'regressionTaskList' and adds them to a list.
#Each model is identifiable by the algorithm for which it is created
createListOfRegressionModels = function(regressionTaskList, mlrLearner){

  regressionModels = list()
  #Iterating over the algorithm names in regressionTaskList so we can use them to identify the models with as well
  for(algorithmName in names(regressionTaskList)){
    newModel = train(mlrLearner, regressionTaskList[[algorithmName]])
    regressionModels[[algorithmName]] = newModel
  }
  return(regressionModels)
}



#Creates a dataframe that the createTask method of MLR can handle
#The data frame contains one element for each feature and an additional element called 'performance' containing the performance values
#The instances considered are all offline instances and the online instances that the specified algorithms has access to
createMlrDataFrame = function(algorithmName, onlineScenario){
  #Methodology: create a dataframe with instance_id's and performances and create another dataframe with instance_id's and feature values.
  #Merge the 2 dataframes on the instance id, thereby assuring the the performances and feature values on each line belong to the same instance
  #Remove the instance id from the dataframe (Otherwise mlr throws an error "unsuppported feature type: character" because it considers the instance_id's a feature)
  
  #Considering both online and offline instances: appending onlineScenario$trainingSet and onlineScenario$onlineInstanceOverview for the selected algorithm
  #It is assumed that the aslibScenario contains performances for all online instances as well as all training instances
  
  performancesDataFrame = getPerformances(algorithmName, c(onlineScenario$trainingSet, onlineScenario$onlineInstanceOverview[[algorithmName]]), onlineScenario$aslibScenario)
  featureValuesDataFrame = getFeatureValuesForInstList(c(onlineScenario$trainingSet, onlineScenario$onlineInstanceOverview[[algorithmName]]), onlineScenario$consideredFeatures, onlineScenario$aslibScenario)
  mlrDataFrameWithInstanceIds = merge(performancesDataFrame,featureValuesDataFrame )
  
  mlrDataFrame = subset(mlrDataFrameWithInstanceIds, TRUE,-instance_id)
  return(mlrDataFrame)
}  

#Creates an mlr regression task for the specified algorithm using data of the specified onlineScenario
createMlrRegressionTask= function(algorithmName, onlineScenario, predTarget = "performance"){
  mlrDataFrame = createMlrDataFrame(algorithmName, onlineScenario)
  mlrTask = makeRegrTask(data = mlrDataFrame, target = predTarget)
  return(mlrTask)
}




