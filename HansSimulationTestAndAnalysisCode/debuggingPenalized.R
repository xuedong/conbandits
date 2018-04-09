library("aslib")
library("mlr")

aslibScenarioName = "QBF-2011"
aslibFolderLoc = "C:\\Users\\u0075355\\Documents\\Onderzoek\\ASLIB\\aslib_data-aslib-v4.0\\"
scenario =  parseASScenario(paste(aslibFolderLoc,aslibScenarioName, sep=""))


algorithmName = "X2clsQ"
instanceList =  c("Qq2k3k3v20v20m80s8","impl18-shuffled","robots_1_5_2_91.8","Qq2k3k3v100v100m400s4" ,           
                  "T-adeu-5" ,"k_branch_p-14-shuffled", "sortnetsort7.v.stepl.004-shuffled", "Qq2k3k3v100v100m1800s7",           
                  "stmt21_143_258" , "robots_1_5_3_78.4", "Qq2k3k3v20v20m320s9", "ii8a1-90-shuffled"                
                  ,"aim-100-3_4-yes1-1-90-shuffled", "robots_1_5_5_61.2") #remove last instance to avoid error
                  
mlrLearner= makeLearner("regr.penalized.ridge",predict.type="response", lambda2=1, model = c("linear"),
                        standardize=FALSE, positive=FALSE)


runtimesDataFrame = subset(scenario$algo.runs, algorithm == algorithmName & instance_id %in% instanceList, select = c(instance_id, runtime))
featureValuesDataFrame = subset(scenario$feature.values, instance_id %in% instanceList, select = c("instance_id", getFeatureNames(scenario)))


#Using mlr: bug
mlrDataFrameWithInstanceIds = merge(runtimesDataFrame,featureValuesDataFrame )
mlrDataFrame = subset(mlrDataFrameWithInstanceIds, TRUE,-instance_id)
mlrTask = makeRegrTask(data = mlrDataFrame, target = "runtime")
newModel = train(mlrLearner,mlrTask) 

#Directly using penalized: same bug
penalized(runtimesDataFrame[,2], penalized=featureValuesDataFrame[2:ncol(featureValuesDataFrame)], standardize=FALSE, 
          positive=FALSE, model=c('linear'), lambda1=0, lambda2=1)






#It does work when standardize = TRUE:
penalized(runtimesDataFrame[,2], penalized=featureValuesDataFrame[2:ncol(featureValuesDataFrame)], standardize=TRUE, 
          positive=FALSE, model=c('linear'), lambda1=0, lambda2=1)








