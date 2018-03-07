createRdsFilesWithAllInstTime = function(reg, scenarios, timeFolder){
  for(scenario in scenarios){

    exp = findExperiments(reg = reg, ids = findDone(), prob.name = scenario)$job.id
   
    dtTime = createIndividualInstPerformanceOverviewOverTimeThesis(reg, exp)
   # dtAvg = createAveragePerTimestepPerformanceOverviewOverTimeThesis(dtTime)
    
    timeFile = paste(timeFolder, '/', scenario, "-allInst.rds", sep = "")
    
    saveRDS(dtTime, file = timeFile)
  }
}

createRdsFilesWithAllInstTimeAvg = function(reg, scenarios, timeFolder){
  for(scenario in scenarios){
    exp = findExperiments(reg = reg, ids = findDone(), prob.name = scenario)$job.id
    
    inputTimeFile = paste(timeFolder, '/', scenario, "-allInst.rds", sep = "")
    
    dtTime = readRDS(inputTimeFile)
    dtAvg = createAveragePerTimestepPerformanceOverviewOverTimeThesis(dtTime)
    
    timeFile = paste(timeFolder, '/', scenario, "-allInst.rds", sep = "")
    
    saveRDS(dtTime, file = timeFile)
  }
}


createRdsFilesWithAllInst = function(reg, scenarios, onlineFolder, verFolder, smVerFolder){
  for(scenario in scenarios){

    exp = findExperiments(reg = reg, ids = findDone(), prob.name = scenario)$job.id
    
    dtOn = createIndividualInstPerformanceOverviewThesis(reg, exp, performanceName = "runtime")
    dtVer = createIndividualInstPerformanceOverviewThesis(reg, exp, performanceName = "verification")
    #dtSmVer = createIndividualInstPerformanceOverviewThesis(reg, exp, performanceName = "selectionMappingVerification")
    
    # dtAvg = createAveragePerTimestepPerformanceOverviewOverTimeThesis(dtTime)
    
    onFile = paste(onlineFolder, '/', scenario, "-allInst.rds", sep = "")
    verFile = paste(verFolder, '/', scenario, "-allInst.rds", sep = "")
    smVerFile = paste(smVerFolder, '/', scenario, "-allInst.rds", sep = "")
    
    
    saveRDS(dtOn, file = onFile)
    saveRDS(dtVer, file = verFile)
    #saveRDS(dtSmVer, file = smVerFile)
  }
}


createRdsFilesWithAllInstAvg = function(reg, scenarios, onlineFolder, verFolder, smVerFolder){
  for(scenario in scenarios){
    exp = findExperiments(reg = reg, ids = findDone(), prob.name = scenario)$job.id
    
    inputOnFile = paste(onlineFolder, '/', scenario, "-allInst.rds", sep = "")
    inputVerFile = paste(verFolder, '/', scenario, "-allInst.rds", sep = "")
    inputSmVerFile = paste(smVerFolder, '/', scenario, "-allInst.rds", sep = "")
    
    dtOnInput = readRDS(inputOnFile)
    dtVerInput = readRDS(inputVerFile)
    dtSmVerInput = readRDS(inputSmVerFile)
    
    dtOnAvg = createAveragePerformanceOverviewThesis(dtOnInput)
    dtVerAvg = createAveragePerformanceOverviewThesis(dtVerInput)
    dtSmVerAvg = createAveragePerformanceOverviewThesis(dtSmVerInput)
    
    
    onFile = paste(onlineFolder, '/', scenario, ".RDS", sep = "")
    verFile = paste(verFolder, '/', scenario, ".RDS", sep = "")
    smVerFile = paste(smVerFolder, '/', scenario, ".RDS", sep = "")
    
    saveRDS(dtOnAvg, file = onFile)
    saveRDS(dtVerAvg, file = verFile)
    saveRDS(dtSmVerAvg, file = smVerFile)
  }
}





createAveragePerTimestepPerformanceOverviewOverTimeThesis = function(individualInstDataTable){
  allExpNrs = unique(individualInstDataTable$expNr)
  
  expNrs = c()
  problemIds = c()
  pTrains = c()
  pOnlines = c()
  pVers = c()
  nrInsts = c()
  algIds = c()
  repls = c()
  timesteps = c()
  vers = c()
  modelRetrainingFreqs = c()
  perfMeasures = c()
  avgPerfs = c()
  avgSbsPerfs = c()
  avgVbsPerfs = c()
  avgSwsPerfs = c()
  avgVwsPerfs = c()
  batchSizes = c()
  mlrLearners = c()
  pOnlineAsTrainings = c()
  
  
  for(expNrValue in allExpNrs){
    thisExpRecords = individualInstDataTable[expNr == expNrValue]
    timestepsThisRecord = unique(thisExpRecords$timestep)
    for(timestepValue in timestepsThisRecord){
      thisTimeStepRecords = thisExpRecords[timestep == timestepValue]
      avgSbsPerf = mean(thisTimeStepRecords$sbsPerf)
      avgVbsPerf = mean(thisTimeStepRecords$vbsPerf)
      avgSwsPerf = mean(thisTimeStepRecords$swsPerf)
      avgVwsPerf = mean(thisTimeStepRecords$vwsPerf)
      avgPerf = mean(thisTimeStepRecords$perf)
      
      avgPerfs = c(avgPerfs, avgPerf)
      avgSbsPerfs = c(avgSbsPerfs, avgSbsPerf)
      avgVbsPerfs = c(avgVbsPerfs, avgVbsPerf)
      avgSwsPerfs = c(avgSwsPerfs, avgSwsPerf)
      avgVwsPerfs = c(avgVwsPerfs, avgVwsPerf)
      
      
      expNrs = c(expNrs, expNrValue)
      timesteps = c(timesteps, timestepValue)
      problemId = unique(thisTimeStepRecords$problemId)
      if(length(problemId)  != 1) {
        stop("None or more than one problem id for a single averaging")
      }
      problemIds = c(problemIds, problemId)
      
      pTrain = unique(thisTimeStepRecords$pTrain)
      if(length(pTrain)  != 1) {
        stop("None or more than one pTrain for a single averaging")
      }
      pTrains = c(pTrains, pTrain)
      
      pOnline = unique(thisTimeStepRecords$pOnline)
      if(length(pOnline)  != 1) {
        stop("None or more than one pOnline for a single averaging")
      }
      pOnlines = c(pOnlines, pOnline)
      
      pVer = unique(thisTimeStepRecords$pVer)
      if(length(pVer)  != 1) {
        stop("None or more than one pVer for a single averaging")
      }
      pVers = c(pVers, pVer)
      
      nrInst = unique(thisTimeStepRecords$nrInst)
      if(length(nrInst)  != 1) {
        stop("None or more than one nrInsts for a single averaging")
      }
      nrInsts = c(nrInsts, nrInst)

      algId = unique(thisTimeStepRecords$algId)
      if(length(algId)  != 1) {
        stop("None or more than one algId for a single averaging")
      }
      algIds = c(algIds, algId)

      repl = unique(thisTimeStepRecords$repl)
      if(length(repl)  != 1) {
        stop("None or more than one repl for a single averaging")
      }
      repls = c(repls, repl)

      ver = unique(thisTimeStepRecords$ver)
      if(length(ver)  != 1) {
        stop("None or more than one ver for a single averaging")
      }
      vers = c(vers, ver)
      
      modelRetrainingFreq = unique(thisTimeStepRecords$modelRetrainingFreq)
      if(length(modelRetrainingFreq)  != 1) {
        stop("None or more than one modelRetrainingFreq for a single averaging")
      }
      modelRetrainingFreqs = c(modelRetrainingFreqs, modelRetrainingFreq)

      perfMeasure = unique(thisTimeStepRecords$perfMeasure)
      if(length(perfMeasure)  != 1) {
        stop("None or more than one perfMeasure for a single averaging")
      }
      perfMeasures = c(perfMeasures, perfMeasure)
      
      batchSize = unique(thisTimeStepRecords$batchSize)
      if(length(batchSize)  != 1) {
        stop("None or more than one batchSize for a single averaging")
      }
      batchSizes = c(batchSizes, batchSize)

      mlrLearner = unique(thisTimeStepRecords$mlrLearner)
      if(length(mlrLearner)  != 1) {
        stop("None or more than one mlrLearner for a single averaging")
      }
      mlrLearners = c(mlrLearners, mlrLearner)
      
      pOnlineAsTraining = unique(thisTimeStepRecords$pOnlineAsTraining)
      if(length(pOnlineAsTraining)  != 1) {
        stop("None or more than one pOnlineAsTraining for a single averaging")
      }
      pOnlineAsTrainings = c(pOnlineAsTrainings, pOnlineAsTraining)
      

      
    }
    
  }
  
  
  #Checking if all input has the same amount of values
  nValues = length(avgVbsPerfs)
  
  if(length(avgSbsPerfs) != nValues){
    stop("avgSbsPerfs invalid nr of values")
  }
  if(length(avgVwsPerfs) != nValues){
    stop("avgVwsPerfs invalid nr of values")
  }
  if(length(avgSwsPerfs) != nValues){
    stop("avgSwsPerfs invalid nr of values")
  }
  if(length(avgPerfs) != nValues){
    stop("avgPerfs invalid nr of values")
  }
  if(length(expNrs) != nValues){
    stop("expNrs invalid nr of values")
  }
  if(length(problemIds) != nValues){
    stop("problemIds invalid nr of values")
  }
  if(length(pTrains) != nValues){
    stop("pTrains invalid nr of values")
  }
  if(length(pOnlines) != nValues){
    stop("pOnlines invalid nr of values")
  }
  if(length(pVers) != nValues){
    stop("pVers invalid nr of values")
  }
  if(length(nrInsts) != nValues){
    stop("nrInsts invalid nr of values")
  }
  if(length(algIds) != nValues){
    stop("algIds invalid nr of values")
  }
  if(length(repls) != nValues){
    stop("repls invalid nr of values")
  }
  if(length(timesteps) != nValues){
    stop("timesteps invalid nr of values")
  }
  if(length(vers) != nValues){
    stop("vers invalid nr of values")
  }
  if(length(modelRetrainingFreqs) != nValues){
    stop("modelRetrainingFreqs invalid nr of values")
  }
  if(length(perfMeasures) != nValues){
    stop("perfMeasures invalid nr of values")
  }
  if(length(batchSizes) != nValues){
    stop("batchSizes invalid nr of values")
  }
  if(length(mlrLearners) != nValues){
    stop("mlrLearners invalid nr of values")
  }
  if(length(pOnlineAsTrainings) != nValues){
    stop("pOnlineAsTrainings invalid nr of values")
  }
  
  
  
  resDt = data.table(problemId = problemIds, algId = algIds, repl = repls, timestep = timesteps,
                     expNr = expNrs, pTrain = pTrains, pOnline = pOnlines, pVer = pVers,
                     pOnlineAsTraining = pOnlineAsTrainings,
                     nrInst = nrInsts, ver = vers, modelRetrainingFreq = modelRetrainingFreqs,
                     batchSize = batchSizes, mlrLearner = mlrLearners, perfMeasure = perfMeasures,
                     avgPerf = avgPerfs, 
                     avgSbsPerf  = avgSbsPerfs, avgVbsPerf = avgVbsPerfs, avgVwsPerf = avgVwsPerfs,
                     avgSwsPerf = avgSwsPerfs)
  setkeyv(resDt, c("problemId", "algId", "repl", "timestep", "pTrain", "pOnline", "pVer", "pOnlineAsTraining"))
  
  #some sanity checks: logical performances for vws, sws, sbs and vbs
  #always true for averages: vws <= sws  <= vbs
  #always true for averages: vws <= sbs  <= vbs
  #always true for averages: vws <= observed perf <= vbs
  #usually true for averages: sws <= sbs (but not always: verification set can contain instances
  # on which the sbs does particularly poorly, or sws particularly well)
  
  if(nrow(resDt[avgVwsPerf > avgSwsPerf]) > 0){
    stop("virtual worst solver better than single worst solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSwsPerf > avgVbsPerf]) > 0){
    stop("Single worst solver better than virtual best solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSbsPerf > avgVbsPerf]) > 0){
    stop("single best solver better than virtual best solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSwsPerf > avgVbsPerf]) > 0){
    stop("single worst solver better than virtual best solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgVwsPerf > avgPerf]) > 0){
    stop("virtual worst solver performance better than observed performance on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgPerf > avgVbsPerf]) > 0){
    stop("Observed performance better than virtual best performance on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSwsPerf > avgSbsPerf]) > 0){
    warning("single worst solver better than single best solver on average. This is unlikely for correct data, 
            but can occur if the virtual best solver (taken over all considered instances) performs particularly
            poorly on the verification instances. Check when this occurs by running resDt[avgSwsPerf > avgSbsPerf], 
            with resDt the data table returned by this function")
  }
  
  return(resDt)
}


createIndividualInstPerformanceOverviewOverTimeThesis = function(reg, experimentNrs, performanceName ="timestep") 
{
  expNrs = c()
  problemIds = c()
  pTrains = c()
  pOnlines = c()
  pVers = c()
  nrInsts = c()
  algIds = c()
  repls = c()
  timesteps = c()
  vers = c()
  modelRetrainingFreqs = c()
  perfMeasures = c()
  perfs = c()
  sbsPerfs = c()
  vbsPerfs = c()
  swsPerfs = c()
  vwsPerfs = c()
  instanceIds = c()
  batchSizes = c()
  mlrLearners = c()
  pOnlineAsTrainings = c()
  
  VERIFICATION_CONSTANT = TRUE
  for (jobId in experimentNrs) {
    jobInfo = getJobTable(reg = reg, ids = jobId)
    result = loadResult(reg = reg, jobId)
    performanceOverview = getElement(result$performanceInfo, performanceName)
    
    
    timepoints = performanceOverview$availableTimesteps
    #timepoints = result$performanceInfo$timestep$availableTimesteps
    
    newVbsPerformances = c()
    newVwsPerformances = c()
    newSbsPerformances = c()
    newSwsPerformances = c()
    newTimeSteps = c()
    newObservedPerformances = c()
    
    for (i in 1:length(timepoints)) {
      #Values that depend on the specific time point
      newVbsPerformances = c(newVbsPerformances,performanceOverview$selectionModelQualityList[[i]]$virtualBest)
      newVwsPerformances = c(newVwsPerformances, performanceOverview$selectionModelQualityList[[i]]$virtualWorst)
      newSbsPerformances = c(newSbsPerformances, performanceOverview$selectionModelQualityList[[i]]$singleBest)
      newSwsPerformances = c(newSwsPerformances, performanceOverview$selectionModelQualityList[[i]]$singleWorst)
      newObservedPerformances = c(newObservedPerformances, performanceOverview$selectionModelQualityList[[i]]$observedPerformance)
      newTimeSteps = c(newTimeSteps, rep(timepoints[[i]], length(performanceOverview$selectionModelQualityList[[i]]$instance_id)))
    }
    vbsPerfs = c(vbsPerfs, newVbsPerformances)
    vwsPerfs = c(vwsPerfs, newVwsPerformances)
    sbsPerfs = c(sbsPerfs, newSbsPerformances)
    swsPerfs = c(swsPerfs, newSwsPerformances)
    perfs = c(perfs, newObservedPerformances)
    timesteps = c(timesteps, newTimeSteps)
    
    
    
    timestepVerInstances = performanceOverview$selectionModelQualityList[[i]]$instance_id
    instanceIds = c(instanceIds, rep(timestepVerInstances, length(timepoints)))
    
    
    nrOfNewEntries = length(newVbsPerformances)                 
    expNrs = c(expNrs, rep(jobInfo$job.id, nrOfNewEntries ))
    problemIds = c(problemIds, rep(jobInfo$problem, nrOfNewEntries))
    pTrains = c(pTrains, rep(jobInfo$prob.pars[[1]]$pInTraining, nrOfNewEntries))
    pOnlines = c(pOnlines, rep(jobInfo$prob.pars[[1]]$pInRuntime, nrOfNewEntries))
    pVers = c(pVers, rep(jobInfo$prob.pars[[1]]$pInVerification, nrOfNewEntries))
    
    nrInsts = c(nrInsts, rep(length(result$onlineScenario$consideredInstances), nrOfNewEntries))
    algIds = c(algIds, rep(jobInfo$algorithm, nrOfNewEntries))
    repls = c(repls, rep(jobInfo$repl, nrOfNewEntries))
    vers = c(vers, rep(VERIFICATION_CONSTANT, nrOfNewEntries))
    
    #Checking if model retraining frequency and batch sizes exist
    if(! is.null(jobInfo$algo.pars[[1]]$nrOfStepsWithoutRetraining)){
      modelRetrainingFreqs = c(modelRetrainingFreqs, rep(jobInfo$algo.pars[[1]]$nrOfStepsWithoutRetraining, nrOfNewEntries))
    }
    else{
      modelRetrainingFreqs = c(modelRetrainingFreqs, rep(Inf, nrOfNewEntries))
    }
    if(! is.null(jobInfo$algo.pars[[1]]$batchSize)){
      batchSizes = c(batchSizes, rep(jobInfo$algo.pars[[1]]$batchSize, nrOfNewEntries))
    }
    else{
      batchSizes = c(batchSizes, rep(Inf, nrOfNewEntries))
    }
    
    mlrLearners = c(mlrLearners, rep(jobInfo$algo.pars[[1]]$mlrLearnerName, nrOfNewEntries))
    perfMeasures = c(perfMeasures, rep(jobInfo$prob.pars[[1]]$performanceMeasure, nrOfNewEntries))
    
    #checking if pOnlineAsTrainings exists
    if(! is.null(jobInfo$algo.pars[[1]]$pOnlineAsTraining)){
      pOnlineAsTrainings = c(pOnlineAsTrainings, rep(jobInfo$algo.pars[[1]]$pOnlineAsTraining, nrOfNewEntries))
    }else{
      pOnlineAsTrainings = c(pOnlineAsTrainings, rep(0, nrOfNewEntries))
    }
    
  }

  #Checking if all input has the same amount of values
  nValues = length(vbsPerfs)
  
  if(length(sbsPerfs) != nValues){
    stop("sbsPerfs invalid nr of values")
  }
  if(length(vwsPerfs) != nValues){
    stop("vwsPerfs invalid nr of values")
  }
  if(length(swsPerfs) != nValues){
    stop("swsPerfs invalid nr of values")
  }
  if(length(expNrs) != nValues){
    stop("expNrs invalid nr of values")
  }
  if(length(problemIds) != nValues){
    stop("problemIds invalid nr of values")
  }
  if(length(pTrains) != nValues){
    stop("pTrains invalid nr of values")
  }
  if(length(pOnlines) != nValues){
    stop("pOnlines invalid nr of values")
  }
  if(length(pVers) != nValues){
    stop("pVers invalid nr of values")
  }
  if(length(nrInsts) != nValues){
    stop("nrInsts invalid nr of values")
  }
  if(length(algIds) != nValues){
    stop("algIds invalid nr of values")
  }
  if(length(repls) != nValues){
    stop("repls invalid nr of values")
  }
  if(length(timesteps) != nValues){
    stop("repls invalid nr of values")
  }
  if(length(vers) != nValues){
    stop("vers invalid nr of values")
  }
  if(length(modelRetrainingFreqs) != nValues){
    stop("modelRetrainingFreqs invalid nr of values")
  }
  if(length(perfMeasures) != nValues){
    stop("perfMeasures invalid nr of values")
  }
  if(length(perfs) != nValues){
    stop("perfs invalid nr of values")
  }
  if(length(instanceIds) != nValues){
    stop("instanceIds invalid nr of values")
  }
  if(length(batchSizes) != nValues){
    stop("batchSizes invalid nr of values")
  }
  if(length(mlrLearners) != nValues){
    stop("mlrLearners invalid nr of values")
  }
  if(length(pOnlineAsTrainings) != nValues){
    stop("pOnlineAsTrainings invalid nr of values")
  }
  
  
  resDt = data.table(problemId = problemIds, algId = algIds, repl = repls, timestep = timesteps,
                     expNr = expNrs, pTrain = pTrains, pOnline = pOnlines, pVer = pVers,
                     pOnlineAsTraining = pOnlineAsTrainings,
                     nrInst = nrInsts, ver = vers, modelRetrainingFreq = modelRetrainingFreqs,
                     batchSize = batchSizes, mlrLearner = mlrLearners, perfMeasure = perfMeasures,
                     instanceId = instanceIds, perf = perfs, 
                     sbsPerf  = sbsPerfs, vbsPerf = vbsPerfs, vwsPerf = vwsPerfs, swsPerf = swsPerfs)
  setkeyv(resDt, c("problemId", "algId", "repl", "timestep", "pTrain", "pOnline", "pVer",
                   "instanceId"))
  
  #some sanity checks: logical performances for vws, sws, sbs and vbs
  #always true for each instance: vws <= sws <= vbs
  #always true for each instance: vws <= sbs <= vbs
  #always true for each instance: vws <= observed perf <= vbs
  
  if(nrow(resDt[vwsPerf > swsPerf]) > 0){
    stop("virtual worst solver better than single worst solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[vwsPerf > sbsPerf]) > 0){
    stop("virtual worst solver better than single best solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[sbsPerf > vbsPerf]) > 0){
    stop("single best solver better than virtual best solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[swsPerf > vbsPerf]) > 0){
    stop("single worst solver better than virtual best solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[vwsPerf > perf]) > 0){
    stop("virtual worst solver performance better than observed performance on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[perf > vbsPerf]) > 0){
    stop("Observed performance better than virtual best performance on > 1 inst. This is impossible for correct data")
  }
  
  
  return(resDt)
}


createIndividualInstPerformanceOverviewThesis = function(reg, experimentNrs, performanceName ="runtime") 
{
  expNrs = c()
  problemIds = c()
  pTrains = c()
  pOnlines = c()
  pVers = c()
  nrInsts = c()
  algIds = c()
  repls = c()
  vers = c()
  modelRetrainingFreqs = c()
  perfMeasures = c()
  perfs = c()
  sbsPerfs = c()
  vbsPerfs = c()
  swsPerfs = c()
  vwsPerfs = c()
  instanceIds = c()
  batchSizes = c()
  mlrLearners = c()
  pOnlineAsTrainings = c()
  
  VERIFICATION_CONSTANT = TRUE
  for (jobId in experimentNrs) {
    jobInfo = getJobTable(reg = reg, ids = jobId)
    result = loadResult(reg = reg, jobId)
    performanceOverview = getElement(result$performanceInfo, performanceName)
    if(is.null(performanceOverview) & jobInfo$algorithm != "offline"){
      stop(paste("Specified performance does not exist: ", performanceName))
    }
    
    #timepoints = performanceOverview$availableTimesteps

    newVbsPerformances = performanceOverview$virtualBest
    newVwsPerformances = performanceOverview$virtualWorst
    newSbsPerformances = performanceOverview$singleBest
    newSwsPerformances =performanceOverview$singleWorst
    newObservedPerformances = performanceOverview$observedPerformance

    vbsPerfs = c(vbsPerfs, newVbsPerformances)
    vwsPerfs = c(vwsPerfs, newVwsPerformances)
    sbsPerfs = c(sbsPerfs, newSbsPerformances)
    swsPerfs = c(swsPerfs, newSwsPerformances)
    perfs = c(perfs, newObservedPerformances)

    usedInstanceIds = performanceOverview$instance_id
    instanceIds = c(instanceIds, usedInstanceIds)
    
    nrOfNewEntries = length(usedInstanceIds)
    expNrs = c(expNrs, rep(jobInfo$job.id, nrOfNewEntries ))
    problemIds = c(problemIds, rep(jobInfo$problem, nrOfNewEntries))
    pTrains = c(pTrains, rep(jobInfo$prob.pars[[1]]$pInTraining, nrOfNewEntries))
    pOnlines = c(pOnlines, rep(jobInfo$prob.pars[[1]]$pInRuntime, nrOfNewEntries))
    pVers = c(pVers, rep(jobInfo$prob.pars[[1]]$pInVerification, nrOfNewEntries))
    
    nrInsts = c(nrInsts, rep(length(result$onlineScenario$consideredInstances), nrOfNewEntries))
    algIds = c(algIds, rep(jobInfo$algorithm, nrOfNewEntries))
    repls = c(repls, rep(jobInfo$repl, nrOfNewEntries))
    vers = c(vers, rep(VERIFICATION_CONSTANT, nrOfNewEntries))
    
    #Checking if model retraining frequency and batch sizes exist
    if(! is.null(jobInfo$algo.pars[[1]]$nrOfStepsWithoutRetraining)){
      modelRetrainingFreqs = c(modelRetrainingFreqs, rep(jobInfo$algo.pars[[1]]$nrOfStepsWithoutRetraining, nrOfNewEntries))
    }
    else{
      modelRetrainingFreqs = c(modelRetrainingFreqs, rep(Inf, nrOfNewEntries))
    }
    if(! is.null(jobInfo$algo.pars[[1]]$batchSize)){
      batchSizes = c(batchSizes, rep(jobInfo$algo.pars[[1]]$batchSize, nrOfNewEntries))
    }
    else{
      batchSizes = c(batchSizes, rep(Inf, nrOfNewEntries))
    }
    
    mlrLearners = c(mlrLearners, rep(jobInfo$algo.pars[[1]]$mlrLearnerName, nrOfNewEntries))
    perfMeasures = c(perfMeasures, rep(jobInfo$prob.pars[[1]]$performanceMeasure, nrOfNewEntries))
    
    #checking if pOnlineAsTrainings exists
    if(! is.null(jobInfo$algo.pars[[1]]$pOnlineAsTraining)){
      pOnlineAsTrainings = c(pOnlineAsTrainings, rep(jobInfo$algo.pars[[1]]$pOnlineAsTraining, nrOfNewEntries))
    }else{
      pOnlineAsTrainings = c(pOnlineAsTrainings, rep(0, nrOfNewEntries))
    }
    
  }
  
  #Checking if all input has the same amount of values
  nValues = length(vbsPerfs)
  
  if(length(sbsPerfs) != nValues){
    stop("sbsPerfs invalid nr of values")
  }
  if(length(vwsPerfs) != nValues){
    stop("vwsPerfs invalid nr of values")
  }
  if(length(swsPerfs) != nValues){
    stop("swsPerfs invalid nr of values")
  }
  if(length(expNrs) != nValues){
    stop("expNrs invalid nr of values")
  }
  if(length(problemIds) != nValues){
    stop("problemIds invalid nr of values")
  }
  if(length(pTrains) != nValues){
    stop("pTrains invalid nr of values")
  }
  if(length(pOnlines) != nValues){
    stop("pOnlines invalid nr of values")
  }
  if(length(pVers) != nValues){
    stop("pVers invalid nr of values")
  }
  if(length(nrInsts) != nValues){
    stop("nrInsts invalid nr of values")
  }
  if(length(algIds) != nValues){
    stop("algIds invalid nr of values")
  }
  if(length(repls) != nValues){
    stop("repls invalid nr of values")
  }
  if(length(vers) != nValues){
    stop("vers invalid nr of values")
  }
  if(length(modelRetrainingFreqs) != nValues){
    stop("modelRetrainingFreqs invalid nr of values")
  }
  if(length(perfMeasures) != nValues){
    stop("perfMeasures invalid nr of values")
  }
  if(length(perfs) != nValues){
    stop("perfs invalid nr of values")
  }
  if(length(instanceIds) != nValues){
    stop("instanceIds invalid nr of values")
  }
  if(length(batchSizes) != nValues){
    stop("batchSizes invalid nr of values")
  }
  if(length(mlrLearners) != nValues){
    stop("mlrLearners invalid nr of values")
  }
  if(length(pOnlineAsTrainings) != nValues){
    stop("pOnlineAsTrainings invalid nr of values")
  }
  
  
  resDt = data.table(problemId = problemIds, algId = algIds, repl = repls,
                     expNr = expNrs, pTrain = pTrains, pOnline = pOnlines, pVer = pVers,
                     pOnlineAsTraining = pOnlineAsTrainings,
                     nrInst = nrInsts, ver = vers, modelRetrainingFreq = modelRetrainingFreqs,
                     batchSize = batchSizes, mlrLearner = mlrLearners, perfMeasure = perfMeasures,
                     instanceId = instanceIds, perf = perfs, 
                     sbsPerf  = sbsPerfs, vbsPerf = vbsPerfs, vwsPerf = vwsPerfs, swsPerf = swsPerfs)
  setkeyv(resDt, c("problemId", "algId", "repl", "pTrain", "pOnline", "pVer",
                   "instanceId"))
  
  #some sanity checks: logical performances for vws, sws, sbs and vbs
  #always true for each instance: vws <= sws <= vbs
  #always true for each instance: vws <= sbs <= vbs
  #always true for each instance: vws <= observed perf <= vbs
  
  if(nrow(resDt[vwsPerf > swsPerf]) > 0){
    stop("virtual worst solver better than single worst solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[vwsPerf > sbsPerf]) > 0){
    stop("virtual worst solver better than single best solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[sbsPerf > vbsPerf]) > 0){
    stop("single best solver better than virtual best solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[swsPerf > vbsPerf]) > 0){
    stop("single worst solver better than virtual best solver on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[vwsPerf > perf]) > 0){
    stop("virtual worst solver performance better than observed performance on > 1 inst. This is impossible for correct data")
  }
  if(nrow(resDt[perf > vbsPerf]) > 0){
    stop("Observed performance better than virtual best performance on > 1 inst. This is impossible for correct data")
  }
  
  
  return(resDt)
}

createAveragePerformanceOverviewThesis = function(individualInstDataTable){
  allExpNrs = unique(individualInstDataTable$expNr)
  
  expNrs = c()
  problemIds = c()
  pTrains = c()
  pOnlines = c()
  pVers = c()
  nrInsts = c()
  algIds = c()
  repls = c()
  vers = c()
  modelRetrainingFreqs = c()
  perfMeasures = c()
  avgPerfs = c()
  avgSbsPerfs = c()
  avgVbsPerfs = c()
  avgSwsPerfs = c()
  avgVwsPerfs = c()
  batchSizes = c()
  mlrLearners = c()
  pOnlineAsTrainings = c()
  
  
  for(expNrValue in allExpNrs){
    thisExpRecords = individualInstDataTable[expNr == expNrValue]
    
    avgSbsPerf = mean(thisExpRecords$sbsPerf)
    avgVbsPerf = mean(thisExpRecords$vbsPerf)
    avgSwsPerf = mean(thisExpRecords$swsPerf)
    avgVwsPerf = mean(thisExpRecords$vwsPerf)
    avgPerf = mean(thisExpRecords$perf)
      
    avgPerfs = c(avgPerfs, avgPerf)
    avgSbsPerfs = c(avgSbsPerfs, avgSbsPerf)
    avgVbsPerfs = c(avgVbsPerfs, avgVbsPerf)
    avgSwsPerfs = c(avgSwsPerfs, avgSwsPerf)
    avgVwsPerfs = c(avgVwsPerfs, avgVwsPerf)
    
    
    expNrs = c(expNrs, expNrValue)
    
    problemId = unique(thisExpRecords$problemId)
    if(length(problemId)  != 1) {
      stop("None or more than one problem id for a single averaging")
    }
    problemIds = c(problemIds, problemId)
    
    pTrain = unique(thisExpRecords$pTrain)
    if(length(pTrain)  != 1) {
      stop("None or more than one pTrain for a single averaging")
    }
    pTrains = c(pTrains, pTrain)
    
    pOnline = unique(thisExpRecords$pOnline)
    if(length(pOnline)  != 1) {
      stop("None or more than one pOnline for a single averaging")
    }
    pOnlines = c(pOnlines, pOnline)
    
    pVer = unique(thisExpRecords$pVer)
    if(length(pVer)  != 1) {
      stop("None or more than one pVer for a single averaging")
    }
    pVers = c(pVers, pVer)
    
    nrInst = unique(thisExpRecords$nrInst)
    if(length(nrInst)  != 1) {
      stop("None or more than one nrInsts for a single averaging")
    }
    nrInsts = c(nrInsts, nrInst)
    
    algId = unique(thisExpRecords$algId)
    if(length(algId)  != 1) {
      stop("None or more than one algId for a single averaging")
    }
    algIds = c(algIds, algId)
    
    repl = unique(thisExpRecords$repl)
    if(length(repl)  != 1) {
      stop("None or more than one repl for a single averaging")
    }
    repls = c(repls, repl)
    
    ver = unique(thisExpRecords$ver)
    if(length(ver)  != 1) {
      stop("None or more than one ver for a single averaging")
    }
    vers = c(vers, ver)
    
    modelRetrainingFreq = unique(thisExpRecords$modelRetrainingFreq)
    if(length(modelRetrainingFreq)  != 1) {
      stop("None or more than one modelRetrainingFreq for a single averaging")
    }
    modelRetrainingFreqs = c(modelRetrainingFreqs, modelRetrainingFreq)
      
    perfMeasure = unique(thisExpRecords$perfMeasure)
    if(length(perfMeasure)  != 1) {
      stop("None or more than one perfMeasure for a single averaging")
    }
    perfMeasures = c(perfMeasures, perfMeasure)
    
    batchSize = unique(thisExpRecords$batchSize)
    if(length(batchSize)  != 1) {
      stop("None or more than one batchSize for a single averaging")
    }
    batchSizes = c(batchSizes, batchSize)
    
    mlrLearner = unique(thisExpRecords$mlrLearner)
    if(length(mlrLearner)  != 1) {
      stop("None or more than one mlrLearner for a single averaging")
    }
    mlrLearners = c(mlrLearners, mlrLearner)
    
    pOnlineAsTraining = unique(thisExpRecords$pOnlineAsTraining)
    if(length(pOnlineAsTraining)  != 1) {
      stop("None or more than one pOnlineAsTraining for a single averaging")
    }
    pOnlineAsTrainings = c(pOnlineAsTrainings, pOnlineAsTraining)

  }
  
  
  #Checking if all input has the same amount of values
  nValues = length(avgVbsPerfs)
  
  if(length(avgSbsPerfs) != nValues){
    stop("avgSbsPerfs invalid nr of values")
  }
  if(length(avgVwsPerfs) != nValues){
    stop("avgVwsPerfs invalid nr of values")
  }
  if(length(avgSwsPerfs) != nValues){
    stop("avgSwsPerfs invalid nr of values")
  }
  if(length(avgPerfs) != nValues){
    stop("avgPerfs invalid nr of values")
  }
  if(length(expNrs) != nValues){
    stop("expNrs invalid nr of values")
  }
  if(length(problemIds) != nValues){
    stop("problemIds invalid nr of values")
  }
  if(length(pTrains) != nValues){
    stop("pTrains invalid nr of values")
  }
  if(length(pOnlines) != nValues){
    stop("pOnlines invalid nr of values")
  }
  if(length(pVers) != nValues){
    stop("pVers invalid nr of values")
  }
  if(length(nrInsts) != nValues){
    stop("nrInsts invalid nr of values")
  }
  if(length(algIds) != nValues){
    stop("algIds invalid nr of values")
  }
  if(length(repls) != nValues){
    stop("repls invalid nr of values")
  }
  if(length(vers) != nValues){
    stop("vers invalid nr of values")
  }
  if(length(modelRetrainingFreqs) != nValues){
    stop("modelRetrainingFreqs invalid nr of values")
  }
  if(length(perfMeasures) != nValues){
    stop("perfMeasures invalid nr of values")
  }
  if(length(batchSizes) != nValues){
    stop("batchSizes invalid nr of values")
  }
  if(length(mlrLearners) != nValues){
    stop("mlrLearners invalid nr of values")
  }
  if(length(pOnlineAsTrainings) != nValues){
    stop("pOnlineAsTrainings invalid nr of values")
  }
  
  
  
  resDt = data.table(problemId = problemIds, algId = algIds, repl = repls,
                     expNr = expNrs, pTrain = pTrains, pOnline = pOnlines, pVer = pVers,
                     pOnlineAsTraining = pOnlineAsTrainings,
                     nrInst = nrInsts, ver = vers, modelRetrainingFreq = modelRetrainingFreqs,
                     batchSize = batchSizes, mlrLearner = mlrLearners, perfMeasure = perfMeasures,
                     avgPerf = avgPerfs, 
                     avgSbsPerf  = avgSbsPerfs, avgVbsPerf = avgVbsPerfs, avgVwsPerf = avgVwsPerfs,
                     avgSwsPerf = avgSwsPerfs)
  setkeyv(resDt, c("problemId", "algId", "repl", "pTrain", "pOnline", "pVer", "pOnlineAsTraining"))
  
  #some sanity checks: logical performances for vws, sws, sbs and vbs
  #always true for averages: vws <= sws  <= vbs
  #always true for averages: vws <= sbs  <= vbs
  #always true for averages: vws <= observed perf <= vbs
  #usually true for averages: sws <= sbs (but not always: verification set can contain instances
  # on which the sbs does particularly poorly, or sws particularly well)
  
  if(nrow(resDt[avgVwsPerf > avgSwsPerf]) > 0){
    stop("virtual worst solver better than single worst solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSwsPerf > avgVbsPerf]) > 0){
    stop("Single worst solver better than virtual best solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSbsPerf > avgVbsPerf]) > 0){
    stop("single best solver better than virtual best solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSwsPerf > avgVbsPerf]) > 0){
    stop("single worst solver better than virtual best solver on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgVwsPerf > avgPerf]) > 0){
    stop("virtual worst solver performance better than observed performance on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgPerf > avgVbsPerf]) > 0){
    stop("Observed performance better than virtual best performance on average. This is impossible for correct data")
  }
  if(nrow(resDt[avgSwsPerf > avgSbsPerf]) > 0){
    warning("single worst solver better than single best solver on average. This is unlikely for correct data, 
            but can occur if the virtual best solver (taken over all considered instances) performs particularly
            poorly on the instances considered (online or verification). Check when this occurs by running resDt[avgSwsPerf > avgSbsPerf], 
            with resDt the data table returned by this function")
  }
  
  return(resDt)
}



