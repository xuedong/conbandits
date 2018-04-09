obtainThesisTimestepPlot = function(performanceDataTable, plotTitle = NULL, benchmarkPerf =  NULL){
  
  normalised=FALSE
  isValidDataTableForTimestepPlot(performanceDataTable)
  if(is.null(plotTitle)){
      plotTitle = paste(performanceDataTable$problemId[[1]])
  }
  
  resPlot = ggplot(data=performanceDataTable, aes(x=timestep, y=avgPerf, colour=as.factor(algId) )) +xlab("Nr of online instances handled")
  if(normalised){
    resPlot = resPlot + ylab("Normalised performance") 
  }else{
    resPlot = resPlot + ylab("Performance") 
  }
  

  #Ensure the y axis goes at least from 0 to 1 in the normalised setting
  if(normalised){
    resPlot = resPlot + expand_limits(y=0) 
    resPlot = resPlot + expand_limits(y=1) 
  }
  
  resPlot = resPlot + theme(legend.title=element_blank()) + theme(axis.text.x = element_text(colour="black", size = axis_font_size), axis.text.y = element_text(colour="black", size = axis_font_size), axis.title.x = element_text(colour="black", size=axis_font_size), axis.title.y = element_text(colour="black", size = axis_font_size))
  resPlot = resPlot + ggtitle(plotTitle) 
  resPlot = resPlot + stat_summary(fun.y=mean, geom="line", size = 1.5)
  resPlot = resPlot + stat_summary( fun.y = uci, geom="line", linetype = "dashed", fun.args = list(nrSd=nrSd), alpha=alpha)
  resPlot = resPlot + stat_summary( fun.y = lci, geom="line", linetype = "dashed", fun.args = list(nrSd=nrSd), alpha=alpha)
  
  #resPlot = resPlot + stat_summary( fun.y = max, geom="line", linetype = "dashed", alpha=alpha)
  #resPlot = resPlot + stat_summary( fun.y = min, geom="line", linetype = "dashed",  alpha=alpha)
  
  
  #  resPlot = resPlot +  stat_summary(fun.y = mean,fun.ymax = lci, fun.ymin = uci, fun.args = list(nrSd=nrSd), alpha = alpha)
  
  if(! is.null(benchmarkPerf)){
    resPlot = resPlot + geom_hline(mapping = aes(colour = "OASC winner", size = 1, yintercept = benchmarkPerf), size = 1.5) 
    resPlot = resPlot + scale_colour_manual(values=c("red", "green")) 
  }
  
  vbsPerf = unique(performanceDataTable$avgVbsPerf)
  sbsPerf = unique(performanceDataTable$avgSbsPerf)
  
  resPlot = resPlot + geom_hline(mapping = aes(colour = "VBS", yintercept = mean(vbsPerf)), size = 1.5) 
  resPlot = resPlot + geom_hline(mapping = aes(colour = "VBS", yintercept = lci(vbsPerf, nrSd = nrSd)), linetype = "dashed") 
  resPlot = resPlot + geom_hline(mapping = aes(colour = "VBS", yintercept = uci(vbsPerf, nrSd = nrSd)), linetype = "dashed") 
  
  resPlot = resPlot + geom_hline(mapping = aes(colour = "SBS", yintercept = mean(sbsPerf)), size = 1.5) 
  resPlot = resPlot + geom_hline(mapping = aes(colour = "SBS", yintercept = lci(sbsPerf, nrSd = nrSd)), linetype = "dashed") 
  resPlot = resPlot + geom_hline(mapping = aes(colour = "SBS", yintercept = uci(sbsPerf, nrSd = nrSd)), linetype = "dashed") 
  
  # resPlot = resPlot + scale_colour_manual(values=c("red", "green")) 
  resPlot = resPlot + scale_colour_brewer(palette = "Set1")#type = "qual", palette  = 2)
  resPlot = resPlot + theme(legend.text=element_text(size=16))
  #print(resPlot)
  return(resPlot) 
  
  
}



isValidDataTableForTimestepPlot = function(performanceDataTable){
  
  if(! is.null(performanceDataTable$instance)){
    stop("The table contains an instance column. Possible you're trying to plot unaveraged performances. The plot function requires 
         for each time point an average performance over all verification instances")
    
  }
  
  problemId = unique(performanceDataTable$problemId)
  if(length(problemId)  != 1) {
    stop("None or more than one problem id")
  }
  
  
  
  pTrain = unique(performanceDataTable$pTrain)
  if(length(pTrain)  != 1) {
    stop("None or more than one pTrain")
  }

  pOnline = unique(performanceDataTable$pOnline)
  if(length(pOnline)  != 1) {
    stop("None or more than one pOnline")
  }

  pVer = unique(performanceDataTable$pVer)
  if(length(pVer)  != 1) {
    stop("None or more than one pVer")
  }

  nrInst = unique(performanceDataTable$nrInst)
  if(length(nrInst)  != 1) {
    stop("None or more than one nrInsts")
  }

  ver = unique(performanceDataTable$ver)
  if(length(ver)  != 1) {
    stop("None or more than one ver")
  }

  perfMeasure = unique(performanceDataTable$perfMeasure)
  if(length(perfMeasure)  != 1) {
    stop("None or more than one perfMeasure ")
  }
  

  return(TRUE)
}



filterOutOfflines = function(performanceDataTable){
  noOffline = performanceDataTable[algId != "offline"]
  return(noOffline)
}


