library(scales)
axis_font_size = 16
value_font_size = 16
#theme_set(theme_grey(base_size = 20))
alpha=0.7
nrSd = 1 
lineSize = 0.7

obtainPar10Plot = function(performanceDataFrame, redLineValue = NULL, plotTitle = NULL, doLogTransformation=FALSE){
  #Assume consistent values for normalised, verification, pTrain, pOnline, pVerification and nrOfInstances over the entire dataframe
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]

  problemId = performanceDataFrame[[1,"problemId"]]
  algIds = performanceDataFrame[,"algorithmId"]
  
  if(is.null(plotTitle)){
    plotTitle = problemId
    if(normalised){
      plotTitle = paste(plotTitle, "normalised") 
    }
    if(verification){
      plotTitle = paste(plotTitle, "verification") 
    }
    else{
      plotTitle = paste(plotTitle, "online") 
    }
    
    #Add proportion of training, online and verification instances
    plotTitle =paste(plotTitle, "\nNr of instances: ", nrOfInstances, sep="")
    plotTitle = paste(plotTitle, "\nTraining-online-verification split: ", round(pTrain*nrOfInstances), "-", round(pOnline*nrOfInstances), "-", round(pVerification*nrOfInstances), sep="")
    

  }
  
  
  resPlot = ggplot(performanceDataFrame, aes(x=algIds, y=PAR10)) +xlab("") + ylab("Normalised performance") 
  
  #Ensure the y axis goes at least from 0 to 1 in the normalised setting
  if(normalised){
    resPlot = resPlot + expand_limits(y=c(0,1)) 
  }
  
  if(! is.null(redLineValue)){
    resPlot = resPlot + geom_hline(mapping = aes(colour = "Llama benchmark", yintercept = redLineValue), size = 1) 
    resPlot = resPlot + scale_colour_manual(values=c("red", "green")) 
  }
  
  resPlot = resPlot+ geom_boxplot() + ggtitle(plotTitle) + theme(legend.title=element_blank()) + theme(axis.text.x = element_text(colour="black", size = axis_font_size), axis.text.y = element_text(colour="black", size = axis_font_size), axis.title.x = element_blank(), axis.title.y = element_text(colour="black", size = axis_font_size))
  if(doLogTransformation){
    resPlot = resPlot + scale_y_continuous(trans=log2_trans())
  }
  
  
  
  return(resPlot) 
  
  
}



#Normalises with sbs score of 1 and vbs score of 0 for all performance measures. Returns a dataframe with same dimensions and col/rownames as performanceDataFrame
normalisePerformance = function(performanceDataFrame, showVbsAndSingleBest = TRUE){
  normalisedPerformanceDataFrame = performanceDataFrame
  
  for(rowNr in rownames(normalisedPerformanceDataFrame)){
    experiment = normalisedPerformanceDataFrame[rowNr,]
    

     if((experiment$algorithmId != "VBS") & (experiment$algorithmId != "singleBest")){
       #Normalise performanc 
       correspondingSingleBestPerf = subset(performanceDataFrame, performanceDataFrame$repl==experiment$repl & performanceDataFrame$algorithmId=="singleBest")[1,]
       correspondingVbsPerf = subset(performanceDataFrame, performanceDataFrame$repl==experiment$repl & performanceDataFrame$algorithmId=="VBS")[1,]
       
       for(perfMeasure in colnames(normalisedPerformanceDataFrame)[4:ncol(normalisedPerformanceDataFrame)]){
         divisor = correspondingSingleBestPerf[[perfMeasure]]-correspondingVbsPerf[[perfMeasure]]
         value = (experiment[[perfMeasure]]-correspondingVbsPerf[[perfMeasure]])/divisor
         normalisedPerformanceDataFrame[[rowNr, perfMeasure]] = value
       }
    }
  }
  
  #Now also change VBs and SBS values to 0 and 1
  if(showVbsAndSingleBest){
    normalisedPerformanceDataFrame[normalisedPerformanceDataFrame$algorithmId=="VBS",4:ncol(normalisedPerformanceDataFrame)] = 0
    normalisedPerformanceDataFrame[normalisedPerformanceDataFrame$algorithmId=="singleBest",4:ncol(normalisedPerformanceDataFrame)] = 1
    
  }
  else{
    normalisedPerformanceDataFrame = normalisedPerformanceDataFrame[! (normalisedPerformanceDataFrame$algorithmId %in% c("VBS", "singleBest")),]
  }
  
  return(normalisedPerformanceDataFrame)
}



obtainOngoingEvaluationPerformance = function(aslibScenarioName){
  performanceList = list()
  performanceList[["ASP-POTASSCO"]] = 137.6
  performanceList[["MAXSAT12-PMS"]] = 322.6
  performanceList[["PREMARSHALLING-ASTAR-2013"]] = 5546.5
  performanceList[["QBF-2011"]] = 1053.7
  performanceList[["SAT11-HAND"]] = 6813.3
  performanceList[["SAT11-INDU"]] = 5650.2
  performanceList[["SAT11-RAND"]] = 1431.1
  performanceList[["CSP-2010"]] = 247.7
  performanceList[["SAT12-RAND"]] = 527.0
  return(performanceList[[aslibScenarioName]])
}

obtainOngoingEvaluationVbs = function(aslibScenarioName){
  performanceList = list()
  performanceList[["ASP-POTASSCO"]] = 21.3
  performanceList[["MAXSAT12-PMS"]] = 40.7
  performanceList[["PREMARSHALLING-ASTAR-2013"]] = 227.6
  performanceList[["QBF-2011"]] = 95.9
  performanceList[["SAT11-HAND"]] = 478.3
  performanceList[["SAT11-INDU"]] = 419.9
  performanceList[["SAT11-RAND"]] = 227.3
  performanceList[["CSP-2010"]] = 107.7
  performanceList[["SAT12-RAND"]] = 46.9
  
  
  return(performanceList[[aslibScenarioName]])
}

obtainOngoingEvaluationSbs = function(aslibScenarioName){
  performanceList = list()
  performanceList[["ASP-POTASSCO"]] = 534.1
  performanceList[["MAXSAT12-PMS"]] = 2111.6
  performanceList[["PREMARSHALLING-ASTAR-2013"]] = 7002.9
  performanceList[["QBF-2011"]] = 9172.3
  performanceList[["SAT11-HAND"]] = 17815.8
  performanceList[["SAT11-INDU"]] = 8985.6
  performanceList[["SAT11-RAND"]] = 14938.6
  performanceList[["CSP-2010"]] = 1087.4
  performanceList[["SAT12-RAND"]] = 568.5
  
  return(performanceList[[aslibScenarioName]])
}

obtainOngoingEvaluationPerformanceNormalised = function(aslibScenarioName){
  performance = obtainOngoingEvaluationPerformance(aslibScenarioName)
  vbs = obtainOngoingEvaluationVbs(aslibScenarioName)
  sbs = obtainOngoingEvaluationSbs(aslibScenarioName)
  
  divisor = sbs-vbs
  value = (performance-vbs)/divisor
  return(value)
}

obtainLlamaBenchmarkPerformanceNormalised = function(aslibScenarioName){
  performance = obtainLlamaBenchmarkPerformance(aslibScenarioName)
  vbs = obtainLlamaBenchmarkPerformanceVbs(aslibScenarioName)
  sbs = obtainLlamaBenchmarkPerformanceSbs(aslibScenarioName)
  
  divisor = sbs-vbs
  value = (performance-vbs)/divisor
  return(value)
}

#According to regr randomForest, PAR10 score:
#http://coseal.github.io/aslib-r/scenario-pages/MAXSAT12-PMS/llama.html
obtainLlamaBenchmarkPerformance = function(aslibScenarioName){
  performanceList = list()
  performanceList[["MAXSAT12-PMS"]] = 3297.740
  performanceList[["QBF-2011"]] = 9104.682
  performanceList[["PREMARSHALLING-ASTAR-2015"]] = 5028.527
  performanceList[["CSP-2010"]] = 6542.550
  performanceList[["SAT11-INDU"]] = 14545.269
  
  if(aslibScenarioName %in% names(performanceList)){
    return(performanceList[[aslibScenarioName]])  
  }
  else{
    return(-1) 
  }
}

#According to regr randomForest, PAR10 score:
#http://coseal.github.io/aslib-r/scenario-pages/MAXSAT12-PMS/llama.html
obtainLlamaBenchmarkPerformanceSbs = function(aslibScenarioName){
  performanceList = list()
  performanceList[["MAXSAT12-PMS"]] = 4893.141
  performanceList[["QBF-2011"]] = 15330.171
  performanceList[["PREMARSHALLING-ASTAR-2015"]] = 7002.907
  performanceList[["CSP-2010"]] = 7201.556
  performanceList[["SAT11-INDU"]] = 14605.904
  
  
  
  

  #performanceList[["PREMARSHALLING-ASTAR-2013"]] = UNKNOWN
  if(aslibScenarioName %in% names(performanceList)){
    return(performanceList[[aslibScenarioName]])  
  }
  else{
    print("No value found for sbs performance in obtainLlamaBenchmarkPerformanceSbs in plotPerformanceHelpers.R")
    return(-1) 
  }
}

#According to regr randomForest, PAR10 score:
#http://coseal.github.io/aslib-r/scenario-pages/MAXSAT12-PMS/llama.html
obtainLlamaBenchmarkPerformanceVbs = function(aslibScenarioName){
  performanceList = list()
  performanceList[["MAXSAT12-PMS"]] = 3127.236
  performanceList[["QBF-2011"]] = 8337.099
  performanceList[["PREMARSHALLING-ASTAR-2015"]] = 227.605
  performanceList[["CSP-2010"]] = 6344.251
  performanceList[["SAT11-INDU"]] = 8187.518
  if(aslibScenarioName %in% names(performanceList)){
    return(performanceList[[aslibScenarioName]])  
  }
  else{
    print("No value found for vbs performance in obtainLlamaBenchmarkPerformanceVbs in plotPerformanceHelpers.R. -1 returned")
    return(-1) 
  }
  
}




createPlotDataFrame = function(reg, jobIdList, verification=FALSE, normalised=FALSE, includeVbs = FALSE, includeSingleBest= FALSE){
  resPerformance = makeDataFrame(nrow = length(jobIdList), ncol = 15, 
                                 col.types = c("numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "character", "numeric", "logical", "logical", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                                 col.names = c("expNr", "problemId", "pTrain", "pOnline", "pVerification", "nrOfInstances", "algorithmId", "repl", "verification", "normalised", "modelRetraingFreq", "PAR10", "avgMisclassificationPenalty", "pSuccess", "PAR1"))
  
  # resPerformance = makeDataFrame(nrow = length(jobIdList), ncol = 7, 
  #                                 col.types = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric"), 
  #                                col.names = c("problemId", "algorithmId", "repl", "PAR10", "avgMisclassificationPenalty", "pSuccess", "PAR1"))
  
  rowCounter=1
  for(jobId in jobIdList){
    jobInfo = getJobTable(reg = reg, ids = jobId)
    result = loadResult(reg = reg, id = jobId)
    
    
    if(verification){
      if(normalised){
        thisPerf = result$performanceInfo$verification$observedPerformance
        sbsPerf = result$performanceInfo$verification$singleBestPerformance
        vbsPerf = result$performanceInfo$verification$vbsPerformance
        performanceValues = getNormalisedPerformance(thisPerf, singleBestPerf = sbsPerf, vbsPerf = vbsPerf) #From evaluatePredictionQualityOverTime
      }
      else{
        performanceValues = result$performanceInfo$verification$observedPerformance
      }
      
    }
    else{
      if(normalised){
        thisPerf = result$performanceInfo$runtime$observedPerformance
        sbsPerf = result$performanceInfo$runtime$singleBestPerformance
        vbsPerf = result$performanceInfo$runtime$vbsPerformance
        performanceValues = getNormalisedPerformance(thisPerf, singleBestPerf = sbsPerf, vbsPerf = vbsPerf)
      }
      else{
        performanceValues = result$performanceInfo$runtime$observedPerformance
      }
    }
    

    resPerformance[rowCounter,] = list(expNr = jobId, problemId = toString(jobInfo$problem), pTrain = result$onlineScenario$pInTraining, pOnline = result$onlineScenario$pInRuntime, 
                                       pVerification = result$onlineScenario$pInVerification, nrOfInstances = length(result$onlineScenario$consideredInstances),   algorithmId = toString(jobInfo$algorithm), repl = jobInfo$repl, 
                                       verification = (verification==TRUE), normalised = (normalised==TRUE), modelRetraingFreq = result$nrOfStepsWithoutRetraining,
                                       PAR10 = performanceValues$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$averageMisclassificationPenalty,pSuccess = performanceValues$proportionOfSuccesses, PAR1 = performanceValues$meanPar1Score)  
                                       #PAR10 = performanceValues$observedPerformance$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$observedPerformance$averageMisclassificationPenalty, pSuccess = performanceValues$observedPerformance$proportionOfSuccesses, PAR1 = performanceValues$observedPerformance$meanPar1Score) 
    #resPerformance[rowCounter,] = list(problemId = jobInfo$prob,algorithmId = jobInfo$algo, repl = jobInfo$rep, PAR10 = performanceValues$observedPerformance$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$observedPerformance$averageMisclassificationPenalty, pSuccess = performanceValues$observedPerformance$proportionOfSuccesses, PAR1 = performanceValues$observedPerformance$meanPar1Score) 
    
    
    rowCounter = rowCounter+1
    
  }
  
  
  
  
  return(resPerformance)
}







obtainPlotForTimestepsRepeatedExperiments = function(performanceDataFrame, plotTitle = NULL){
  if(!isConsistentValuesForTimestepRepeatedExperimentsPlot(performanceDataFrame)){
    return("Error: The passed performanceDataFrame contains inconsistent values")
  }
  
  
  
  #Assume consistent values for normalised, verification, pTrain, pOnline, pVerification and nrOfInstances over the entire dataframe
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]
  

  problemId = performanceDataFrame[[1,"problemId"]]
  algorithmId = performanceDataFrame[[1,"algorithmId"]]
  
  timesteps = performanceDataFrame[,"timestep"]
  
  if(is.null(plotTitle)){
    plotTitle = paste(problemId, algorithmId)
    if(normalised){
      plotTitle = paste(plotTitle, "normalised") 
    }
    plotTitle = paste(plotTitle, " timestep plot")
    
    #Add proportion of training, online and verification instances
    plotTitle =paste(plotTitle, "\nNr of instances: ", nrOfInstances, sep="")
    plotTitle = paste(plotTitle, "\nTraining-online-verification split: ", round(pTrain*nrOfInstances), "-", round(pOnline*nrOfInstances), "-", round(pVerification*nrOfInstances), sep="")
  }
  
  resPlot = ggplot(data=performanceDataFrame, aes(x=timesteps, y=PAR10)) +xlab("") + ylab("Normalised performance") 
  #Ensure the y axis goes at least from 0 to 1 in the normalised setting
  if(normalised){
    resPlot = resPlot + expand_limits(y=c(0,1)) 
  }
 
  resPlot = resPlot+ geom_line(mapping= aes(colour=as.factor(repl)), alpha=alpha) + ggtitle(plotTitle) + theme(legend.title=element_blank()) + theme(axis.text.x = element_text(colour="black", size = axis_font_size), axis.text.y = element_text(colour="black", size = axis_font_size), axis.title.x = element_blank(), axis.title.y = element_text(colour="black", size = axis_font_size))
  resPlot = resPlot + stat_summary(fun.y=mean, geom="line",colour="green")
 # resPlot = resPlot + stat_summary( fun.y = uci, geom="line", colour="green",fun.args = list(nrSd=nrSd), alpha = 0.5)
 # resPlot = resPlot + stat_summary( fun.y = lci, geom="line", colour="green",fun.args = list(nrSd=nrSd),  alpha = 0.5)
  resPlot = resPlot +  stat_summary(fun.y = mean,fun.ymax = lci, fun.ymin = uci, fun.args = list(nrSd=nrSd), alpha = 0.2, colour="green")
  
   
  
  return(resPlot) 
  
  
}

#Plots the dots corresponding to the performance 
obtainPlotForTimestepsSingleExperiment = function(performanceDataFrame, plotTitle = NULL){
  if(!isConsistentValuesForTimestepSingleExperimentPlot(performanceDataFrame)){
    return("Error: The passed performanceDataFrame contains information on more than one experiment or contains other inconsistencies")
  }
  #Assume consistent values for normalised, verification, pTrain, pOnline, pVerification and nrOfInstances over the entire dataframe
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]
  
  problemId = performanceDataFrame[[1,"problemId"]]
  algorithmId = performanceDataFrame[[1,"algorithmId"]]
  
  timesteps = performanceDataFrame[,"timestep"]
  
  if(is.null(plotTitle)){
    plotTitle = paste(problemId, algorithmId)
    if(normalised){
      plotTitle = paste(plotTitle, "normalised") 
    }
    plotTitle = paste(plotTitle, " timestep plot")
    
    #Add proportion of training, online and verification instances
    plotTitle =paste(plotTitle, "\nNr of instances: ", nrOfInstances, sep="")
    plotTitle = paste(plotTitle, "\nTraining-online-verification split: ", round(pTrain*nrOfInstances), "-", round(pOnline*nrOfInstances), "-", round(pVerification*nrOfInstances), sep="")
  }
  
  resPlot = ggplot(performanceDataFrame, aes(x=timesteps, y=PAR10)) +xlab("") + ylab("avg PAR10") 
  #Ensure the y axis goes at least from 0 to 1 in the normalised setting
  if(normalised){
    resPlot = resPlot + expand_limits(y=c(0,1)) 
  }
  
  #Ensure the y axis goes at least from 0 to 1 in the normalised setting
  if(normalised){
    resPlot = resPlot + expand_limits(y=c(0,1)) 
  }
  
  
  
  resPlot = resPlot+ geom_line() + ggtitle(plotTitle) + theme(legend.title=element_blank()) + theme(axis.text.x = element_text(colour="black", size = axis_font_size), axis.text.y = element_text(colour="black", size = axis_font_size), axis.title.x = element_blank(), axis.title.y = element_text(colour="black", size = axis_font_size))
  
  
  
  return(resPlot) 
  
}

obtainPlotForTimestepsMultipleAlgorithms = function(performanceDataFrame, plotTitle = NULL, redLineValue=NULL){
  if(!isConsistentValuesForTimestepMultipleAlgorithmsPlot(performanceDataFrame)){
    return("Error: The passed performanceDataFrame contains inconsistent values")
  }
  
  #Assume consistent values for normalised, verification, pTrain, pOnline, pVerification and nrOfInstances over the entire dataframe
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]
  problemId = performanceDataFrame[[1,"problemId"]]
  timesteps = performanceDataFrame[,"timestep"]
  
  if(is.null(plotTitle)){
    plotTitle = paste(problemId)
    if(normalised){
      plotTitle = paste(plotTitle, "normalised") 
    }
    plotTitle = paste(plotTitle, " timestep plot")
    
    #Add proportion of training, online and verification instances
    plotTitle =paste(plotTitle, "\nNr of instances: ", nrOfInstances, sep="")
    plotTitle = paste(plotTitle, "\nTraining-online-verification split: ", round(pTrain*nrOfInstances), "-", round(pOnline*nrOfInstances), "-", round(pVerification*nrOfInstances), sep="")
  }
  
  resPlot = ggplot(data=performanceDataFrame, aes(x=timesteps, y=PAR10, colour=as.factor(algorithmId) )) +xlab("Iteration") + ylab("Normalised performance") 
  #Ensure the y axis goes at least from 0 to 1 in the normalised setting
  if(normalised){
    resPlot = resPlot + expand_limits(y=c(0,1)) 
  }

  
  resPlot = resPlot + ggtitle(plotTitle) + theme(legend.title=element_blank()) + theme(axis.text.x = element_text(colour="black", size = axis_font_size), axis.text.y = element_text(colour="black", size = axis_font_size), axis.title.x = element_text(colour="black", size=axis_font_size), axis.title.y = element_text(colour="black", size = axis_font_size))

  resPlot = resPlot + stat_summary(fun.y=mean, geom="line")
  resPlot = resPlot + stat_summary( fun.y = uci, geom="line", fun.args = list(nrSd=nrSd), alpha=alpha)
  resPlot = resPlot + stat_summary( fun.y = lci, geom="line", fun.args = list(nrSd=nrSd), alpha=alpha)
#  resPlot = resPlot +  stat_summary(fun.y = mean,fun.ymax = lci, fun.ymin = uci, fun.args = list(nrSd=nrSd), alpha = alpha)
  
  if(! is.null(redLineValue)){
    resPlot = resPlot + geom_hline(mapping = aes(colour = "Llama benchmark", yintercept = redLineValue), size = 1) 
    #resPlot = resPlot + scale_colour_manual(values=c("red", "green")) 
  }
  
  return(resPlot) 
  
  
}


#Checks if all values in the passed performanceDataFrame have the same values for normalised, verification, pTrain, pOnline pVerification and nrOfInstances
isConsistentValuesForTimestepRepeatedExperimentsPlot = function(performanceDataFrame){
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]
  algorithmId = performanceDataFrame$algorithmId[[1]]
  problemId = performanceDataFrame$problemId[[1]]
  
  for(i in 1:nrow(performanceDataFrame)){
    if(normalised != performanceDataFrame$normalised[[i]]){
      print("Inconsistent normalised")
      return(FALSE) 
    }
    if(verification != performanceDataFrame$verification[[i]]){
      print("Inconsistent verification")
      return(FALSE) 
    }
    if(pTrain != performanceDataFrame$pTrain[[i]]){
      print("Inconsistent pTrain")
      return(FALSE) 
    }
    if(pOnline != performanceDataFrame$pOnline[[i]]){
      print("Inconsistent pOnline")
      return(FALSE)
    }
    if(pVerification != performanceDataFrame$pVerification[[i]]){
      print("Inconsistent pVerification")
      return(FALSE)
    }
    if(nrOfInstances != performanceDataFrame$nrOfInstances[[i]]){
      print("inconsistent nrOfInstances")
      return(FALSE)
    }
    if(algorithmId != performanceDataFrame$algorithmId[[i]]){
      print("inconsistent algorithmId")
      return(FALSE)
    }
    if(problemId != performanceDataFrame$problemId[[i]]){
      print("inconsistent problem id")
      return(FALSE)
    }
  }
  return(TRUE)
}


#Plots the dots corresponding to the performance 
obtainRegressionModelEvolutionOfAvgErrorPlot = function(differenceDataFrame, algorithmName = NULL, plotTitle = NULL){

  if(is.null(plotTitle)){
    plotTitle = paste("Evolution of avg error of \n regression model predictions over time\n")
    if(! is.null(algorithmName)){
      plotTitle = paste(plotTitle, algorithmName) 
    }
  }
  
  resPlot = ggplot(differenceDataFrame, aes(x=timepoint, y=avgDifference)) +xlab("") + ylab("avg error") 
  #Ensure the y axis goes at least from 0 to 1 in the normalised setting

 
  
  resPlot = resPlot+ geom_point() + ggtitle(plotTitle) + theme(legend.title=element_blank()) + theme(axis.text.x = element_text(colour="black", size = axis_font_size), axis.text.y = element_text(colour="black", size = axis_font_size), axis.title.x = element_blank(), axis.title.y = element_text(colour="black", size = axis_font_size))
  
  
  
  return(resPlot) 
  
}




#Checks if all values in the passed performanceDataFrame have the same values for replication, normalised, verification, pTrain, pOnline pVerification and nrOfInstances
isConsistentValuesForTimestepSingleExperimentPlot= function(performanceDataFrame){
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]
  replication = performanceDataFrame$repl[[1]]
  algorithmId = performanceDataFrame$algorithmId[[1]]
  problemId = performanceDataFrame$problemId[[1]]
  
  for(i in 1:nrow(performanceDataFrame)){
    if(normalised != performanceDataFrame$normalised[[i]]){
      print("Inconsistent normalised")
      return(FALSE) 
    }
    if(verification != performanceDataFrame$verification[[i]]){
      print("Inconsistent verification")
      return(FALSE) 
    }
    if(pTrain != performanceDataFrame$pTrain[[i]]){
      print("Inconsistent pTrain")
      return(FALSE) 
    }
    if(pOnline != performanceDataFrame$pOnline[[i]]){
      print("Inconsistent pOnline")
      return(FALSE)
    }
    if(pVerification != performanceDataFrame$pVerification[[i]]){
      print("Inconsistent pVerification")
      return(FALSE)
    }
    if(nrOfInstances != performanceDataFrame$nrOfInstances[[i]]){
      print("Inconsistent nrOfInstances")
      return(FALSE)
    }
    if(replication != performanceDataFrame$repl[[i]]){
      print("Inconsistent replication")
      return(FALSE)
    }
    if(algorithmId != performanceDataFrame$algorithmId[[i]]){
      print("inconsistent algorithmId")
      return(FALSE)
    }
    if(problemId != performanceDataFrame$algorithmId[[i]]){
      print("inconsistent problem id")
      return(FALSE)
    }
    
    
  }
  return(TRUE)
  
}

isConsistentValuesForTimestepMultipleAlgorithmsPlot = function(performanceDataFrame){
  normalised = performanceDataFrame$normalised[[1]]
  verification = performanceDataFrame$verification[[1]]
  pTrain = performanceDataFrame$pTrain[[1]]
  pOnline = performanceDataFrame$pOnline[[1]]
  pVerification = performanceDataFrame$pVerification[[1]]
  nrOfInstances = performanceDataFrame$nrOfInstances[[1]]
  problemId = performanceDataFrame$problemId[[1]]
  
  for(i in 1:nrow(performanceDataFrame)){
    if(normalised != performanceDataFrame$normalised[[i]]){
      print("Inconsistent normalised")
      return(FALSE) 
    }
    if(verification != performanceDataFrame$verification[[i]]){
      print("Inconsistent verification")
      return(FALSE) 
    }
    if(pTrain != performanceDataFrame$pTrain[[i]]){
      print("Inconsistent pTrain")
      return(FALSE) 
    }
    if(pOnline != performanceDataFrame$pOnline[[i]]){
      print("Inconsistent pOnline")
      return(FALSE)
    }
    if(pVerification != performanceDataFrame$pVerification[[i]]){
      print("Inconsistent pVerification")
      return(FALSE)
    }
    if(nrOfInstances != performanceDataFrame$nrOfInstances[[i]]){
      print(nrOfInstances)
      print(performanceDataFrame$nrOfInstances[[i]])
      print(i)
      print("inconsistent nrOfInstances")
      return(FALSE)
    }
    if(problemId != performanceDataFrame$problemId[[i]]){
      print("inconsistent problem id")
      return(FALSE)
    }
  }
  return(TRUE)
}




#Helpers for plotting confidence intervals. From http://stackoverflow.com/questions/12033319/plot-mean-and-sd-of-dataset-per-x-value-using-ggplot2
uci = function(y,nrSd){
  mean(y) + nrSd * sd(y)
}
lci = function(y,nrSd){
  mean(y) - nrSd * sd(y)
}














createPlotDataFrameAllScenarios = function(reg, jobIdNamedList, verification=FALSE){
  normalised = TRUE
  resPerformance = makeDataFrame(nrow = length(jobIdList), ncol = 14, 
                                 col.types = c("numeric", "numeric", "numeric", "numeric",  "numeric", "character", "numeric", "logical", "logical", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                                 col.names = c("problemId", "pTrain", "pOnline", "pVerification", "nrOfInstances", "algorithmId", "repl", "verification", "normalised", "modelRetraingFreq", "PAR10", "avgMisclassificationPenalty", "pSuccess", "PAR1"))
  
  # resPerformance = makeDataFrame(nrow = length(jobIdList), ncol = 7, 
  #                                 col.types = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric"), 
  #                                col.names = c("problemId", "algorithmId", "repl", "PAR10", "avgMisclassificationPenalty", "pSuccess", "PAR1"))
  
  rowCounter=1
  for(jobId in jobIdList){
    jobInfo = getJobTable(reg = reg, ids = jobId)
    result = loadResult(reg = reg, id = jobId)
    
    
    if(verification){
      if(normalised){
        thisPerf = result$performanceInfo$verification$observedPerformance
        sbsPerf = result$performanceInfo$verification$singleBestPerformance
        vbsPerf = result$performanceInfo$verification$vbsPerformance
        performanceValues = getNormalisedPerformance(thisPerf, singleBestPerf = sbsPerf, vbsPerf = vbsPerf) #From evaluatePredictionQualityOverTime
      }
      else{
        performanceValues = result$performanceInfo$verification$observedPerformance
      }
      
    }
    else{
      if(normalised){
        thisPerf = result$performanceInfo$runtime$observedPerformance
        sbsPerf = result$performanceInfo$runtime$singleBestPerformance
        vbsPerf = result$performanceInfo$runtime$vbsPerformance
        performanceValues = getNormalisedPerformance(thisPerf, singleBestPerf = sbsPerf, vbsPerf = vbsPerf)
      }
      else{
        performanceValues = result$performanceInfo$runtime$observedPerformance
      }
    }
    
    
    resPerformance[rowCounter,] = list(problemId = toString(jobInfo$problem), pTrain = result$onlineScenario$pInTraining, pOnline = result$onlineScenario$pInRuntime, 
                                       pVerification = result$onlineScenario$pInVerification, nrOfInstances = length(result$onlineScenario$consideredInstances),   algorithmId = toString(jobInfo$algorithm), repl = jobInfo$repl, 
                                       verification = (verification==TRUE), normalised = (normalised==TRUE), modelRetraingFreq = result$nrOfStepsWithoutRetraining,
                                       PAR10 = performanceValues$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$averageMisclassificationPenalty,pSuccess = performanceValues$proportionOfSuccesses, PAR1 = performanceValues$meanPar1Score)  
    #PAR10 = performanceValues$observedPerformance$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$observedPerformance$averageMisclassificationPenalty, pSuccess = performanceValues$observedPerformance$proportionOfSuccesses, PAR1 = performanceValues$observedPerformance$meanPar1Score) 
    #resPerformance[rowCounter,] = list(problemId = jobInfo$prob,algorithmId = jobInfo$algo, repl = jobInfo$rep, PAR10 = performanceValues$observedPerformance$meanPar10Score, avgMisclassificatioNPenalty = performanceValues$observedPerformance$averageMisclassificationPenalty, pSuccess = performanceValues$observedPerformance$proportionOfSuccesses, PAR1 = performanceValues$observedPerformance$meanPar1Score) 
    
    
    rowCounter = rowCounter+1
    
  }
  
  
  
  
  return(resPerformance)
}

