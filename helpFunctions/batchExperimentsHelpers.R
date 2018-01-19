#For batchtools. Changes standard values to given values.
#data contains an onlineScenario object
onlineScenarioSubsampleFunction = function(data, job, pInTraining, pInRuntime, pInVerification, performanceMeasure, mlrLearnerName){
 
  onlineScenario = data
  oldOnlineScenario =  onlineScenario
  newOnlineScenario = createOnlineScenario(oldOnlineScenario$aslibScenario, oldOnlineScenario$consideredAlgorithms, oldOnlineScenario$consideredInstances, oldOnlineScenario$consideredFeatures,
                                           pInTraining,pInRuntime, pInVerification, performanceMeasure = performanceMeasure)
  
  
  return(newOnlineScenario)
  #onlineLearnerData = initialiseOnlineLearnerData(onlineScenario = newOnlineScenario, mlrLearner=mlrLearner, nrOfStepsWithoutRetraining = onlineLearnerData$nrOfStepsWithoutRetraining, keepOldRegressionTasks = onlineLearnerData$keepOldRegressionTasks, doTimeDependentVerification=onlineLearnerData$doTimeDependentVerification, doTimeDependentRegressionModelEvaluation =onlineLearnerData$doTimeDependentRegressionModelEvaluation )
  #return(onlineLearnerData)  
}

#Resample the training, runtime and verification sets according to the values specified in the passed onlineLearnerData object
#Used within 1 job to resample for the repetitions
#Note: Probably this resampling can be done a lot more straightforward
# insideSubSampleFunctionOnlineLearnerData = function(instance){
#     print("Beginning insideSubSampleFunctionOnlineLearnerData in batchExperimentsHelpers.R")
#     oldOnlineScenario = instance$onlineScenario
#     newOnlineScenario = createOnlineScenario(oldOnlineScenario$aslibScenario, oldOnlineScenario$consideredAlgorithms, oldOnlineScenario$consideredInstances, oldOnlineScenario$consideredFeatures, oldOnlineScenario$pInTraining,oldOnlineScenario$pInRuntime, oldOnlineScenario$pInVerification)
#     print("post createOnlineScenario in insideSubSampleFunctionOnlineLearnerData")
#     onlineLearnerData = initialiseOnlineLearnerData(onlineScenario = newOnlineScenario, mlrLearner=instance$mlrLearner, nrOfStepsWithoutRetraining = instance$nrOfStepsWithoutRetraining, keepOldRegressionTasks = instance$keepOldRegressionTasks, doTimeDependentVerification=instance$doTimeDependentVerification, doTimeDependentRegressionModelEvaluation = instance$doTimeDependentRegressionModelEvaluation )
#     print("Ending insideSubSampleFunctionOnlineLearnerData in batchExperimentsHelpers.R")
#     return(onlineLearnerData)
# }


#Creates a result dataframe containing a line for every jobId
#Assumes the onlineLearnerData object is loaded into the environment with name 'result'
# loadOnlineLearnerData = function(jobId, registryMapLocation){
#   if(jobId<10){
#     onlineLearnerDataObjectName = paste(registryLocation, "/jobs/0", jobId,"/", jobId, "-result.RData", sep="")   
#   }
#   else if(jobId<100){
#     onlineLearnerDataObjectName = paste(registryLocation, "/jobs/", jobId,"/", jobId, "-result.RData", sep="")   
#   }
#   else{
#     print("Unknown folder name for id>100. Check folder name and adapt code")
#   }
#   load(onlineLearnerDataObjectName)
#   return(result)
# }
# 
# 
# #Returns the experiments from the registry that are 
# #finished,
# #for problem with name 'problemName'
# #and with pTraining, pOnline and pVerificaiton as specified
# #For all algorithms specified in listOfAlgorithms
# filterExperiments = function(reg, problemName, pTraining, pOnline, pVerification, listOfAlgorithms){
#   listOfExperiments = c()
#   for(algorithm in listOfAlgorithms){
#     allRelevantProblems = findExperiments(reg, ids = findDone(reg), prob.pattern= problemName, prob.pars = (pInTraining == pTraining && pInRuntime==pOnline && pInVerification==pVerification), algo.pattern = algorithm, match.substring = FALSE)  
#     listOfExperiments = c(listOfExperiments, allRelevantProblems)                        
#   }
#   return(listOfExperiments)
# }
# 
# 
# 
