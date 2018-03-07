#defining a standard registry, containing all problems and algorithms
library("batchtools")

#step 1: initialise the registry
regName = "linUcbTestReg"



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
  aslibFileName = getAslibFilesMapLocation()
  
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
    
    
    
    #onlineLearnerData = initialiseEmptyOnlineLearnerData(onlineScenario = onlineScenario, nrOfStepsWithoutRetraining = nrOfStepsWithoutRetraining, keepOldRegressionTasks = keepOldRegressionTasks,
    #                                                     doTimeDependentVerification = doTimeDependentVerification, doTimeDependentRegressionModelEvaluation = doTimeDependentRegressionModelEvaluation)
    
    addProblem(reg = reg, name=aslibScenarioName, data = onlineScenario, fun = onlineScenarioSubsampleFunction, seed = 1)
    
  }
}

addAlgorithmsToReg = function(reg){
  addAlgorithm(reg=reg, name="greedy", fun = simulateGreedy)
#  addAlgorithm(reg=reg, name="greedyFI", fun = simulateGreedyFullInfo)
#  addAlgorithm(reg=reg, name="eGreedy", fun = simulateEpsilonGreedy)
  addAlgorithm(reg=reg, name="random", fun = simulateEpsilonGreedy)
  addAlgorithm(reg=reg, name="ucbProper", fun=simulateUcbProper)
#  addAlgorithm(reg=reg, name="ucbOld", fun=simulateUcbProper)
  addAlgorithm(reg = reg, name = "offline", fun = simulateOffline)
#  addAlgorithm(reg = reg, name = "eFirst", fun= simulateEpsilonFirst)
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

source("utils_disjoint.R")
source("linucb_disjoint.R")


sourceFiles = getAllRFilenamesInDir("helpFunctions")
sourceFiles = c(sourceFiles, "LinUcbIntegrationInSimulation.R", "utils_disjoint.R", "linucb_disjoint.R")

packagesList = c("BBmisc", "ParamHelpers","checkmate", "data.table", "ggplot2","methods","parallelMap","stats",
                 "stringi", "aslib", "batchtools", "mlr", "xlsx", "randomForest")#, "ranger")

reg <- makeExperimentRegistry(regName, seed=1, packages = packagesList, source=sourceFiles)








wantedScenarioNames = c("QBF-2011")
pdPerScenarioList = readRDS("pdPerScenarioList.rds")
adPerAlgorithmList = readRDS("adPerScenarioList.rds")

linUcbAd = data.table(doTimeDependentVerification = TRUE, batchSize = 10, alpha0 = 0, mlrLearnerName = "noMlrLearner")
adPerAlgorithmList$linUcb = linUcbAd

addProblemsToReg(reg, wantedScenarioNames)
addAlgorithmsToReg(reg)


wantedAlgorithms = c("greedy", "random", "linUcb")

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

#Wait until completion (~2 min for CPMP-2015)




#Result analysis
#comparing results
greedyResRf = loadResult(1)
linUcbRes = loadResult(3)
randomRes = loadResult(2)


print("Online performance")
greedyRfOn = mean(greedyResRf$performanceInfo$runtime$observedPerformance)
linUcbOn = mean(linUcbRes$performanceInfo$runtime$observedPerformance)
randomOn = mean(randomRes$performanceInfo$runtime$observedPerformance)
sbsOn = mean(linUcbRes$performanceInfo$runtime$singleBest)
vbsOn = mean(linUcbRes$performanceInfo$runtime$virtualBest)
print(paste("Sbs ", sbsOn))
print(paste("Vbs ", vbsOn))
print(paste("greedy Random forest ", greedyRfOn))
print(paste("linUcb ", linUcbOn))
print(paste("random" , randomOn))


print("Verification performance")
greedyRfVer = mean(greedyResRf$performanceInfo$verification$observedPerformance)
linUcbVer = mean(linUcbRes$performanceInfo$verification$observedPerformance)
randomVer = mean(randomRes$performanceInfo$verification$observedPerformance)
sbsVer = mean(linUcbRes$performanceInfo$verification$singleBest)
vbsVer = mean(linUcbRes$performanceInfo$verification$virtualBest)
print(paste("Sbs ", sbsVer))
print(paste("Vbs ", vbsVer))
print(paste("greedy Random forest ", greedyRfVer))
print(paste("linUcb ", linUcbVer))
print(paste("random" , randomVer))







source("plotHelpFunctions/thesisDataTableCreationCode.R")
source("plotHelpFunctions/thesisPlotCreation.R")






allInstDt = createIndividualInstPerformanceOverviewOverTimeThesis(reg, findDone()$job.id)
avgPerfDt = createAveragePerTimestepPerformanceOverviewOverTimeThesis(allInstDt)

obtainThesisTimestepPlot(avgPerfDt[problemId == "QBF-2011"])

createRdsFilesWithAllInst(reg, scenarios, onlineFolder = onlineFolder, verFolder = verFolder,
                                   smVerFolder = smVerFolder)
createRdsFilesWithAllInstTime(reg, scenarios, timeFolder)

#averages
createRdsFilesWithAllInstTimeAvg(reg, scenarios, timeFolder)


onlineFolder = paste(mainFolder, "online/", sep = "/")
verFolder = paste(mainFolder, "verification/", sep = "/")
timeFolder = paste(mainFolder, "time/", sep = "/")
smVerFolder = paste(mainFolder, "selMapVerification/", sep = "/")


scenarioName = "CPMP-2015"
timeDt = readRDS(paste(timeFolder,scenarioName,"-allInst.RDS", sep=""))
#onDt = readRDS(paste(onlineFolder,scenarioName,".RDS", sep=""))



