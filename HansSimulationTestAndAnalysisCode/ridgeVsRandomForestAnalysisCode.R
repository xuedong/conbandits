library(xlsx)
library("batchtools")

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir("plotHelpFunctions")

reg = loadRegistry("ridgeRegrReg", writeable=TRUE)
syncRegistry(reg)



mainFolder = paste(regName, "resultsRidgeRegrVsRandomForest", sep = "-")
onlineFolder = paste(mainFolder, "online", sep = "/")
offlineFolder = paste(mainFolder, "offline", sep = "/")

if(!dir.exists(file.path(onlineFolder))){
  dir.create(file.path(onlineFolder), recursive=TRUE)
}
if(!dir.exists(file.path(offlineFolder))){
  dir.create(file.path(offlineFolder), recursive=TRUE)
}

scenarios = reg$problems



createRdsFilesWithAllInst(reg, scenarios, onlineFolder = onlineFolder)
createRdsFilesWithAllInstAvg(reg, scenarios, onlineFolder = onlineFolder)

#Create 1 data.frame with all scenarios under each other
folder = onlineFolder
allDtsUnderEachOther = NULL
for(scenario in scenarios){
  perfDf = data.table(readRDS(paste(folder, "/", scenario, ".rds", sep="")))
  
  if(is.null(allDtsUnderEachOther)){
    allDtsUnderEachOther = perfDf
  }else{
    allDtsUnderEachOther = rbind(allDtsUnderEachOther, perfDf)
  }
}


allDtsUnderEachOtherNormalised = allDtsUnderEachOther
allDtsUnderEachOtherNormalised$avgPerf = (allDtsUnderEachOtherNormalised$avgPerf - allDtsUnderEachOtherNormalised$avgSbsPerf)/(allDtsUnderEachOtherNormalised$avgVbsPerf-allDtsUnderEachOtherNormalised$avgSbsPerf)

#Which scenarios to consider (only the 'healthy' ones for algorithm selection are used)
scenarioSubset = c("ASP-POTASSCO","BNSL-2016",
                   "CPMP-2015",
                   "CSP-2010",
                   "CSP-MZN-2013",
                   "MAXSAT12-PMS",
                   "PROTEUS-2014",
                   "QBF-2011",
                   "QBF-2014",
                   "SAT11-HAND",
                   "SAT11-RAND",
                   "SAT12-ALL",
                   "SAT12-HAND")
                   
allDtsUnderEachOtherNormalised = allDtsUnderEachOtherNormalised[problemId %in% scenarioSubset,]

#analysis 1: comparing the performance of ridge regr and random forest for online algorithm selection 





# rFPerfs = c()
# ridgePerfs = c()
# vbsPerfs = c()
# sbsPerfs = c()
# scenarioNames = c()
# meanPerformancesPerScenario = list()
# 
# 
# for(scenario in scenarios){
#   perfDf = data.table(readRDS(paste(folder, "/", scenario, ".rds", sep="")))
#
#   meanPerformances = list()
#   for(mlrLearnerValue in unique(perfDf$mlrLearner)){
#     relevantDf = perfDf[mlrLearner == mlrLearnerValue,]
#     meanPerf = mean(relevantDf$avgPerf)
#     meanPerformances[[mlrLearnerValue]] = meanPerf
#   }
#   
#   #calculated this way to allow its use when not all replications for all algorithms were successful
#   vbsValues = c()
#   sbsValues = c()
#   for(replValue in unique(perfDf$repl)){
#     replDf = perfDf[repl == replValue,]
#     if(length(unique(replDf$VBSPar10)) > 1){
#       print(replDf)
#       stop(paste("Different vbs values for repl", repl, "in scenario", scenario))
#     }
#     if(length(unique(replDf$SBSPar10))>1){
#       print(replDf)
#       stop(paste("Different sbs values for repl", repl, "in scenario", scenario))
#     }  
#     vbsValues = c(vbsValues, replDf$avgVbsPerf)
#     sbsValues = c(sbsValues, replDf$avgSbsPerf)
#   }
#   
#   meanPerformances[["sbs"]] = mean(sbsValues)
#   meanPerformances[["vbs"]] = mean(vbsValues)
#   
#   meanPerformancesPerScenario[[scenario]] = meanPerformances
#   
#   
# }
# 
# 
# 
# perfDataTable = data.table(scenario = scenarios)
# 
# for(scenarioValue in scenarios){
#   for(alg in names(meanPerformancesPerScenario[[scenarioValue]])){
#     perfDataTable[scenario == scenarioValue,alg] = meanPerformancesPerScenario[[scenarioValue]][[alg]]
#   }
# }



#creating plot
perfPerScenarioPlot =   ggplot(data=allDtsUnderEachOtherNormalised, aes(x=problemId, y=avgPerf, colour=as.factor(mlrLearner) )) +xlab("scenario name") + ylab("normalised perf (sbs = 0; vbs = 1)")
perfPerScenarioPlot = perfPerScenarioPlot + geom_boxplot()
perfPerScenarioPlor = perfPerScenarioPlot + ggtitle("Average performance of random forest and glmnet ridge regression for greedy online alg sel with warm start of 10% (10 repetitions)")
print(perfPerScenarioPlot)







