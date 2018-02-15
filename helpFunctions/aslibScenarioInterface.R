#Loads the aslib scenario with name 'aslibScenarioName'
#Creates an onlineScenario object containing all algorithms and features of the scenario
loadAslibScenarioIntoOnlineScenario = function(aslibScenarioName, pInTraining,pInRuntime, pInVerification, performanceMeasure){
  scenario = parseASScenario(paste(getAslibFilesMapLocation(),aslibScenarioName, sep=""))
  relevantFeatureList = getFeatureNames(scenario)
  relevantAlgorithmList = getAlgorithmNames(scenario)
  relevantInstanceList = getInstanceNames(scenario)
  onlineScenario = createOnlineScenario(aslibScenario = scenario, consideredAlgorithms = relevantAlgorithmList, 
                                        consideredInstances = relevantInstanceList, consideredFeatures = relevantFeatureList,
                                        pInTraining=pInTraining, pInRuntime=pInRuntime, pInVerification=pInVerification,
                                        performanceMeasure = performanceMeasure)
  return(onlineScenario)
}

loadAslibScenarioIntoOnlineScenarioWithTop3Alg = function(aslibScenarioName, pInTraining,pInRuntime, pInVerification, performanceMeasure){
  scenario = parseASScenario(paste(getAslibFilesMapLocation(),aslibScenarioName, sep=""))
  relevantFeatureList = getFeatureNames(scenario)
  relevantAlgorithmList = getTop3AlgorithmsInScenario(scenario)
  relevantInstanceList = getInstanceNames(scenario)
  onlineScenario = createOnlineScenario(aslibScenario = scenario, consideredAlgorithms = relevantAlgorithmList, consideredInstances = relevantInstanceList, 
                                        consideredFeatures = relevantFeatureList, pInTraining=pInTraining, pInRuntime=pInRuntime, pInVerification=pInVerification,
                                        performanceMeasure = performanceMeasure)
  return(onlineScenario)
}

loadAslibScenarioIntoOnlineScenarioWith3MostCompatibleAlg = function(aslibScenarioName, pInTraining,pInRuntime, pInVerification, performanceMeasure){
  scenario = parseASScenario(paste(getAslibFilesMapLocation(),aslibScenarioName, sep=""))
  relevantFeatureList = getFeatureNames(scenario)
  relevantAlgorithmList = get3MostCompatibleAlgorithmsInScenario(scenario)
  if(aslibScenarioName == "CSP-2010"){ #only 2 alg. Third one is NA
    relevantAlgorithmList = relevantAlgorithmList[1:2]
  }
  relevantInstanceList = getInstanceNames(scenario)
  onlineScenario = createOnlineScenario(aslibScenario = scenario, consideredAlgorithms = relevantAlgorithmList, consideredInstances = relevantInstanceList, 
                                        consideredFeatures = relevantFeatureList, pInTraining=pInTraining, pInRuntime=pInRuntime, pInVerification=pInVerification,
                                        performanceMeasure = performanceMeasure)
  return(onlineScenario)
}

getTop3AlgorithmsInScenario = function(scenario){
  #scenario = parseASScenario(paste(getAslibFilesMapLocation(),aslibScenarioName, sep=""))
  
  means = c()
  algorithms = c()
  for(algorithm in unique(scenario$algo.runs$algorithm)){
    runs = scenario$algo.runs[scenario$algo.runs$algorithm == algorithm,]
    runs$par10 = runs$runtime
    runs[runs$runstatus != "ok",]$par10 = runs[runs$runstatus != "ok",]$par10*10
    meanPar10 = mean(runs$par10)
    means = c(means, meanPar10)
    algorithms = c(algorithms, algorithm)
  }
  
  df = data.frame(means, algorithms)
  dfArranged  = arrange(df, means)
  if(nrow(dfArranged)> 2){
    top3Alg = dfArranged$algorithms[1:3]
  }
  else{
    top3Alg = dfArranged$algorithms
  }
  return(top3Alg)
}

get3MostCompatibleAlgorithmsInScenario = function(scenario){
  best3Alg = readRDS('3bestAlgsOverview.rds')
  algorithms = best3Alg[best3Alg == scenario$desc$scenario_id,]  
  
  res = c(algorithms[1,'algorithm'], algorithms[2,'algorithm'], algorithms[3,'algorithm'])
}


#Returns the map that contains the maps with aslib scenarios. 
#Introduced for easy adaptation to other computers
getAslibFilesMapLocation = function(){
  return("C:\\Users\\u0075355\\Documents\\Onderzoek\\ASLIB\\aslib_data-aslib-v4.0\\")
  #return("C:\\Users\\u0075355\\Documents\\Onderzoek\\ASLIB\\aslib_data-master_jan-2017\\aslib_data-master\\")
  #return("/home/hpc/pr74ze/di36gac/hans_test/aslib_data-master/")
  
}

