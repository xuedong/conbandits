wantedAlgorithms = c("greedy")
adPerAlgorithmList = readRDS("adPerScenarioList.rds")
pdPerScenarioList = readRDS("pdPerScenarioList.rds")

adPerAlgorithmList$greedy$mlrLearnerName = "regr.glmnet" 

wantedScenarioNames = "QBF-2011"
wantedScenarioNames = reg$problems

wantedScenarioNames = wantedScenarioNames[1:26]

adL = list()
for(algorithm in wantedAlgorithms){
  adL[[algorithm]] = adPerAlgorithmList[[algorithm]]
}


pdL = list()
for(scenario in wantedScenarioNames){
  pdL[[scenario]] = pdPerScenarioList[[scenario]]
}

nrOfRep = 10
addExperiments(prob.designs = pdL, algo.designs = adL, repls = nrOfRep)




notDone = setdiff(findExperiments()$job.id, findDone()$job.id)
notSubmitted = setdiff(notDone, findSubmitted()$job.id)
resourceReqOverview = data.table(read.xlsx("resourceRequiredPerExperiment.xlsx", sheetIndex = 1))

#Submission for all 
for(scenarioName in wantedScenarioNames){
  ids =  findExperiments(ids = notSubmitted, prob.name = scenarioName )$job.id
  fIExp = findExperiments(ids = ids, algo.name = "greedyFI")$job.id
  offlineExp = findExperiments(ids = ids, algo.name = "offline")$job.id
  #excluding offline and fullInfo
  otherExp = setdiff(ids, fIExp)
  otherExp = setdiff(otherExp, offlineExp)
  
  
  walltime = resourceReqOverview[scenario == scenarioName]$walltime
  memory = resourceReqOverview[scenario == scenarioName]$memory
  
  walltimeFI = resourceReqOverview[scenario == scenarioName]$walltimeFI
  memoryFI = resourceReqOverview[scenario == scenarioName]$memoryFI
  
  walltimeOff = resourceReqOverview[scenario == scenarioName]$walltimeOff
  memoryOff = resourceReqOverview[scenario == scenarioName]$memoryOff
  
  
  submitJobs(ids = otherExp, resources = list(walltime = walltime, memory = memory))
  submitJobs(ids = fIExp, resources = list(walltime = walltimeFI, memory = memoryFI))
  submitJobs(ids = offlineExp,resources = list(walltime = walltimeOff, memory = memoryOff))
  
}


