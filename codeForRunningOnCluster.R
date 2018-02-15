#Script that can be used to run on a cluster (can also be tested on home computer)
#to test on home pc: only 1 problem and a subset of the available algorithms are considered, for 1 repetition


#defining a standard registry, containing all problems and algorithms
library("batchtools")
aslibFilesMapLocation = "aslib_data-master"
#step 1: initialise the registry
regName = "testReg"

getAllFilenamesInDir <- function(path) {
  allFilenames = c()
  for (nm in list.files(path) ){
    filepath = file.path(path, nm)
    allFilenames = c(allFilenames, filepath)
    
    
  }
  return(allFilenames)
}

#Returns all scenario names in an aslib master folder
#Works by considering all file name sin the folder and ignoring the readme
getAllScenarioNames = function(){
  aslibFileName = aslibFilesMapLocation
  
  allProblems = getAllFilenamesInDir(aslibFileName)
  splitProblems = strsplit(allProblems, split = "/")
  problemNames = vector(mode = "character", length = length(splitProblems)-1)
  counter=1
  for(i in 1:length(splitProblems)){
    if( splitProblems[[i]][[length(splitProblems[[i]])]] != "README.md"){
      problemNames[[counter]] = splitProblems[[i]][[length(splitProblems[[i]])]]
      counter = counter+1
    }
  }
  return(problemNames)
}



addProblemsToReg = function(reg, scenarioNames){
  pInRuntime= 0.1
  pInVerification=0.8
  pInTraining=0.1
  pdPerScenarioList = readRDS("pdPerScenarioList.rds")
  
  
  for(aslibScenarioName in scenarioNames){
    #print(aslibScenarioName)
    performanceMeasure = pdPerScenarioList[[aslibScenarioName]]$performanceMeasure
    
    
    onlineScenario = loadAslibScenarioIntoOnlineScenario(aslibScenarioName = aslibScenarioName,pInTraining=pInTraining, 
                                                         pInRuntime = pInRuntime, pInVerification = pInVerification, performanceMeasure = performanceMeasure)
    
    addProblem(reg = reg, name=aslibScenarioName, data = onlineScenario, fun = onlineScenarioSubsampleFunction, seed = 1)
  }
}

addAlgorithmsToReg = function(reg){
  addAlgorithm(reg=reg, name="greedy", fun = simulateGreedy)
  addAlgorithm(reg=reg, name="greedyFI", fun = simulateGreedyFullInfo)
  addAlgorithm(reg=reg, name="eGreedy", fun = simulateEpsilonGreedy)
  addAlgorithm(reg=reg, name="random", fun = simulateEpsilonGreedy)
  addAlgorithm(reg=reg, name="ucb", fun=simulateUcb)
  addAlgorithm(reg = reg, name = "offline", fun = simulateOffline)
  addAlgorithm(reg = reg, name = "eFirst", fun= simulateEpsilonFirst)
  addAlgorithm(reg = reg, name = "linUcb", fun = simulateLinUcb)
}


getAllRFilenamesInDir <- function(path) {
  allFilenames = c()
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    filepath = file.path(path, nm)
    allFilenames = c(allFilenames, filepath)
    
    
  }
  return(allFilenames)
}



sourceFiles = getAllRFilenamesInDir("helpFunctions")
sourceFiles = c(sourceFiles, "linUcbIntegrationInSimulation.R", "linucb_disjoint.R")

packagesList = c("BBmisc", "ParamHelpers","checkmate", "data.table", "ggplot2","methods","parallelMap","stats",
                 "stringi", "aslib", "batchtools", "mlr", "xlsx", "randomForest")

reg <- makeExperimentRegistry(regName, seed=1, packages = packagesList, source=sourceFiles)

wantedScenarioNames = c("CPMP-2015") #getAllScenarioNames()
scenariosToIgnore = c("TSP-LION2015")
wantedScenarioNames = setdiff(wantedScenarioNames, scenariosToIgnore)
pdPerScenarioList = readRDS("pdPerScenarioList.rds")
adPerAlgorithmList = readRDS("adPerScenarioList.rds")

linUcbAdDataTable = data.table(  doTimeDependentVerification = TRUE,batchSize = 10, alpha0 = 0)


adPerAlgorithmList$linUcb = linUcbAdDataTable


addProblemsToReg(reg, wantedScenarioNames)
addAlgorithmsToReg(reg)

wantedAlgorithms = c("greedy", "eGreedy", "ucb", "offline", "random", "eFirst", "linUcb")
adL = list()
for(algorithm in wantedAlgorithms){
  adL[[algorithm]] = adPerAlgorithmList[[algorithm]]
}

pdL = list()
for(scenario in wantedScenarioNames){
  pdL[[scenario]] = pdPerScenarioList[[scenario]]
}

nrOfRep = 1
addExperiments(prob.designs = pdL, algo.designs = adL, repls = nrOfRep)

submitJobs()


