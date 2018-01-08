#Methods related to creating and updating an onlineScenario object
library("BBmisc")


#Bundles all data required for an online scenario.
#Creates and returns an S3 object consisting of:
#	aslibScenario: the aslib scenario containing the runtime data on which the simulation is based
#	consideredInstances: the instances considered during the experiments
#	consideredAlgorithms: the algorithms considered in the simulation
#	consideredFeatures: the features considered in the simulation
#	onlineInstanceOverview: an overview for each algorithm of which online instances it has access to
#llamaScenario: the equivalent of aslibScenario that is interpretable by the code from the R-package LLAMA
#llamaScenarioVerificationOnly: a llama-interpretable scenario consisting only of the instances in the verification set
createOnlineScenario = function(aslibScenario, consideredAlgorithms, consideredInstances, consideredFeatures, pInTraining,pInRuntime, pInVerification){
  onlineInstanceOverview = initialiseOnlineInstanceOverview(consideredAlgorithms)
  onlineScenario = makeS3Obj("onlineScenario", aslibScenario = aslibScenario, consideredInstances=consideredInstances, consideredAlgorithms = consideredAlgorithms, onlineInstanceOverview = onlineInstanceOverview, consideredFeatures = consideredFeatures, pInTraining=pInTraining, pInRuntime=pInRuntime, pInVerification=pInVerification)
  onlineScenario = createTrainingRuntimeAndVerificationInstanceSets(onlineScenario)
  
  onlineScenario$llamaScenario = convertToLlama(aslibScenario) #This code sometimes throws 'warning: 'Warning in convertToCheck(asscenario, measure, feature.steps, TRUE) :  #Requested to add feature costs, but none in scenario. Adding always 0 feature costs.'
  #Creating the llamaScenario based only on the verification instances
  verificationOnlyScenario = createSubsetOfAslibScenario(aslibScenario, onlineScenario$verificationSet)
  onlineScenario$llamaScenarioVerificationOnly = convertToLlama(verificationOnlyScenario)
  #Creating the llamaScenario based only on the runtime instances
  runtimeOnlyScenario = createSubsetOfAslibScenario(aslibScenario, onlineScenario$runtimeSet)
  onlineScenario$llamaScenarioOnlineOnly = convertToLlama(runtimeOnlyScenario)
  
  return(onlineScenario)
}


#Creates an empty list of lists in which will be stored for each algorithm for which instances it has online performance data
#Structure: list identifiable by algorithm name. Each list-element refers to an empty list, which will contain the names of the instances it has solved)
initialiseOnlineInstanceOverview = function(algorithmList){
  onlineInstanceList = list()
  for(algorithmName in algorithmList){
    onlineInstanceList[[algorithmName]] = list()
  }
  return(onlineInstanceList)
}

#Splits the set of consideredInstances into a training set, a runtime set and a verification set. 
#pInTraining, pInRuntime and pInVerification are the probabilities of an instance belonging to the training set, runtime set and verification set
#pInTraining + pInRuntime + pInVerification <= 1! (if smaller than 1 some instances are not considered in the experiments)
#Note: in most cases not all available instances will be considered due to the floor operation
createTrainingRuntimeAndVerificationInstanceSets = function(onlineScenario){
  allInstancesRandomised = sample(onlineScenario$consideredInstances)
  nrOfInstances = length(allInstancesRandomised)
  nrOfTrainingInstances = floor(nrOfInstances*onlineScenario$pInTraining)  #floor operator to guarantee sufficient instances exist
  nrOfRuntimeInstances = floor(nrOfInstances*onlineScenario$pInRuntime)
  nrOfVerificationInstances = floor(nrOfInstances*onlineScenario$pInVerification)
  
  trainingSetStartIndex = 1
  trainingSetEndIndex = nrOfTrainingInstances
  runtimeSetStartIndex = trainingSetEndIndex+1
  runtimeSetEndIndex = runtimeSetStartIndex+nrOfRuntimeInstances-1
  verificationSetStartIndex = runtimeSetEndIndex+1
  verificationSetEndIndex = verificationSetStartIndex+nrOfVerificationInstances-1
  
  if(nrOfTrainingInstances < 1){
    onlineScenario$trainingSet = list()
  }
  else{
    onlineScenario$trainingSet = allInstancesRandomised[trainingSetStartIndex:trainingSetEndIndex]
  }
  onlineScenario$runtimeSet = allInstancesRandomised[runtimeSetStartIndex:runtimeSetEndIndex]
  onlineScenario$verificationSet = allInstancesRandomised[verificationSetStartIndex:verificationSetEndIndex]
  
  return(onlineScenario)
  
}




#Adds the specified instance to the list of online solved instances (specified by onlineInstanceOverviewList) of the specified algorithm
addOnlineInstanceToAlgorithm = function(instanceId, algorithmName, onlineScenario){
  

  onlineScenario$onlineInstanceOverview[[algorithmName]] = c(unlist(onlineScenario$onlineInstanceOverview[[algorithmName]]), instanceId)
  return(onlineScenario)
}
