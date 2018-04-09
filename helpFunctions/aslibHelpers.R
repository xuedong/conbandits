#Helper methods for accessing data from an aslib scenario and transforming aslib scenarios
library("data.table")

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


#Adds a column to the algo.runs data.frame of an aslib scenario that shows the values for PAR10 performance
#Assumes there exists a column called' runtime' containing the runtimes
addPar10PerformanceToAslibScenario= function(aslibScenario){
  aslibScenario$algo.runs$PAR10 = aslibScenario$algo.runs$runtime
  timeoutIndices = aslibScenario$algo.runs$runstatus != 'ok'
  timeoutPerformance = aslibScenario$desc$algorithm_cutoff_time*10
  aslibScenario$algo.runs$PAR10[timeoutIndices] = timeoutPerformance
  return(aslibScenario)
}

#Adds a column to the algo.runs data.frame of an aslib scenario that shows the values for runtime performance
#Assumes there exists a column called' PAR10' containing the PAR10 values
addRuntimePerformanceToAslibScenario= function(aslibScenario){
  aslibScenario$algo.runs$runtime = aslibScenario$algo.runs$PAR10
  timeoutIndices = aslibScenario$algo.runs$runstatus != 'ok'
  timeoutPerformance = aslibScenario$desc$algorithm_cutoff_time
  aslibScenario$algo.runs$runtime[timeoutIndices] = timeoutPerformance
  return(aslibScenario)
}



#Creates the 'performance' column in aslibScenario$algo.runs and fills it with the desired performanceMeasure
#PAR10 case: creates a PAR10 score column if it does not yet exist
#All performances are maximised. If it is standard minimised (as with PAR10/runtime),
#the performances are transformed to turn them into maximisation performances
addPerformanceColumnToAslibScenario = function(aslibScenario, performanceMeasure){
  colNames = names(aslibScenario$algo.runs)
  if(performanceMeasure == "PAR10"){
    if(! ("PAR10" %in% colNames | "runtime" %in% colNames)){
      stop("No PAR10 performance column can be created for the aslibScenario; column called 'runtime' or 'PAR10' is required")
    }
    if(! "PAR10" %in% colNames){
      aslibScenario = addPar10PerformanceToAslibScenario(aslibScenario)
    }
    timeoutLimit = aslibScenario$desc$algorithm_cutoff_time
    aslibScenario$algo.runs$performance = 10*timeoutLimit-aslibScenario$algo.runs$PAR10
    aslibScenario$desc$algorithm_max_perf = 10*timeoutLimit
    aslibScenario$desc$algorithm_min_perf = 0
    return(aslibScenario)
  }
  if(performanceMeasure == "runtime"){
    if(! ("PAR10" %in% colNames | "runtime" %in% colNames)){
      stop("No runtime performance column can be created for the aslibScenario; column called 'runtime' or 'PAR10' is required")
    }
    if(! "runtime" %in% colNames){
      aslibScenario = addRuntimePerformanceToAslibScenario(aslibScenario)
    }
    timeoutLimit = aslibScenario$desc$algorithm_cutoff_time
    aslibScenario$algo.runs$performance = timeoutLimit-aslibScenario$algo.runs$runtime
    aslibScenario$desc$algorithm_max_perf = timeoutLimit
    aslibScenario$desc$algorithm_min_perf = 0
    return(aslibScenario)
    
  }
  
  #For CSP-Minizind-Obj-2016
  #Best performance: value 0
  #Worst performance: value 1
  if(performanceMeasure == "obj"){
    aslibScenario$algo.runs$performance = 1-aslibScenario$algo.runs$obj
    aslibScenario$desc$algorithm_max_perf = 1
    aslibScenario$desc$algorithm_min_perf = 0
    return(aslibScenario)
  }
  
  
  
  
  #other cases: simply paste required column to performanceMeasure
  #Note: assumes it's a maximisation performance. If not, must be transformed
  #Note: assumes the min and max performance are both observed
  if (! (performanceMeasure %in% colNames)){
    stop(paste("Desired performance", performanceMeasure, "not found in the columns of aslibScenario$algo.runs"))
  }
  else{
    warning(paste("Performance measure", performanceMeasure, " has no code written for it in function addPerformanceColumnToAslibScenario in aslibHelpers.R. The values of the performance 
                  were taken directly. So it is assumed that the performance is a maximisation performance, and that the minimal and maximal performance value are both present in the algorithm
                  runs."))
  }
  aslibScenario$algo.runs$performance = aslibScenario$algo.runs[,performanceMeasure]
  aslibScenario$desc$algorithm_max_perf = max(aslibScenario$algo.runs$performance)
  aslibScenario$desc$algorithm_min_perf = min(aslibScenario$algo.runs$performance)
  return(aslibScenario)
  
  
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

#Returns a dataframe with 2 columns: 1 with instance_id's and another with the performances of the specified algorithm on them.
#The instances considered are listed in instanceList
getPerformances = function(algorithmId, instanceList, scenario){
  performancessDataFrame = subset(scenario$algo.runs, algorithm == algorithmId & instance_id %in% instanceList, select = c(instance_id, performance))
  return(performancessDataFrame)    
}

#returns the single best solver as measured by the 'performance' column of the algo.runs in aslibScenario
#Calculated based on consideredInstances and consideredAlgorithms
getSingleBestSolver = function(aslibScenario, consideredInstances, consideredAlgorithms){
  bestAlgorithm = NULL
  bestMeanPerf = -1
  for(algorithm in consideredAlgorithms){
    performances = aslibScenario$algo.runs[aslibScenario$algo.runs$instance_id %in% consideredInstances
                                           & aslibScenario$algo.runs$algorithm == algorithm,]$performance
    meanPerf = mean(performances)
    if(is.null(bestAlgorithm) | meanPerf > bestMeanPerf){
      bestAlgorithm = algorithm
      bestMeanPerf = meanPerf
    }
  }
  return(bestAlgorithm)
}  
  
#returns the single worst solver as measured by the 'performance' column of the algo.runs in aslibScenario
#This is the algorithm with wordst average performance
#Calculated based on consideredInstances and consideredAlgorithms
getSingleWorstSolver = function(aslibScenario, consideredInstances, consideredAlgorithms){
  worstAlgorithm = NULL
  worstMeanPerf = -1
  for(algorithm in consideredAlgorithms){
    performances = aslibScenario$algo.runs[aslibScenario$algo.runs$instance_id %in% consideredInstances
                                           & aslibScenario$algo.runs$algorithm == algorithm,]$performance
    meanPerf = mean(performances)
    if(is.null(worstAlgorithm) | meanPerf < worstMeanPerf){
      worstAlgorithm = algorithm
      worstMeanPerf = meanPerf
    }
  }
  return(worstAlgorithm)
}  

#Returns the performance of the virtual best solver on the given instances
#Considering the specified algorithms
getVirtualBestSolverPerformance = function(aslibScenario, instanceList, consideredAlgorithms){
  performancesList = c()
  for(instance in instanceList){
    bestPerf =  max(aslibScenario$algo.runs[aslibScenario$algo.runs$instance_id == instance & 
                                              aslibScenario$algo.runs$algorithm %in% consideredAlgorithms,]$performance)
    performancesList = c(performancesList, bestPerf)
  }
  resultTable = data.table("instance_id" = instanceList, "performance" = performancesList)
  
  return(resultTable)
}

#Returns the performance of the virtual best solver on the given instances
#Considering the specified algorithms
getVirtualWorstSolverPerformance = function(aslibScenario, instanceList, consideredAlgorithms){
 performancesList = c()
 for(instance in instanceList){
   worstPerf =  min(aslibScenario$algo.runs[aslibScenario$algo.runs$instance_id == instance & 
                                              aslibScenario$algo.runs$algorithm %in% consideredAlgorithms,]$performance)
   performancesList = c(performancesList, worstPerf)
 }
 resultTable = data.table("instance_id" = instanceList, "performance" = performancesList)
 
 return(resultTable)
 
}

