#Helper methods for accessing data from an aslib scenario and transforming aslib scenarios


#Creates a new aslibScenario based on the provided, but throws out all instances that are unsolved by any of the algorithms
limitScenarioToSolvedInstances = function(aslibScenario){
  llamaScenario = convertToLlama(aslibScenario)
  successOverview = successes(llamaScenario, vbs, addCosts = FALSE)
  solvedInstances = subset(llamaScenario$data$instance_id, successOverview==TRUE)
  subsetAslibScenario = createSubsetOfAslibScenario(aslibScenario, solvedInstances)
  return(subsetAslibScenario)
}

#Creates a subset of the data in the aslibScenario by limiting it to the instances defined in instanceSubset
createSubsetOfAslibScenario = function(aslibScenario, instanceSubset){
  subsetAslibScenario = aslibScenario
  subsetAslibScenario$feature.runstatus = subset(subsetAslibScenario$feature.runstatus, subsetAslibScenario$feature.runstatus$instance_id %in% instanceSubset)
  subsetAslibScenario$feature.values = subset(subsetAslibScenario$feature.values, subsetAslibScenario$feature.values$instance_id %in% instanceSubset)
  subsetAslibScenario$feature.costs = subset(subsetAslibScenario$feature.costs, subsetAslibScenario$feature.costs$instance_id %in% instanceSubset)
  subsetAslibScenario$algo.runs = subset(subsetAslibScenario$algo.runs, subsetAslibScenario$algo.runs$instance_id %in% instanceSubset)
  subsetAslibScenario$algo.runstatus = subset(subsetAslibScenario$algo.runstatus, subsetAslibScenario$algo.runstatus$instance_id %in% instanceSubset)
  subsetAslibScenario$cv.splits = subset(subsetAslibScenario$cv.splits, subsetAslibScenario$cv.splits$instance_id %in% instanceSubset)
  #Note: splits are probably not even anymore when subset
  
  
  return(subsetAslibScenario)
}


#Returns a dataframe with 1+len(featureNameList) columns. The first column contains the instance_id. The following columns contain the feature values specified in 'featureNameList'
#Only instances in 'instanceList' are considered
getFeatureValuesForInstList = function(instanceList, featureIdList, scenario){
  featureValuesDataFrame = subset(scenario$feature.values, instance_id %in% instanceList, select = c("instance_id", featureIdList))
  return (featureValuesDataFrame)
}


#Returns a dataframe with 2 columns: 1 with instance_id's and another with the runtime of the specified algorithm on them.
#The instances considered are listed in instanceList
getRuntimes = function(algorithmId, instanceList, scenario){
  runtimesDataFrame = subset(scenario$algo.runs, algorithm == algorithmId & instance_id %in% instanceList, select = c(instance_id, runtime))
  return(runtimesDataFrame)    
}
