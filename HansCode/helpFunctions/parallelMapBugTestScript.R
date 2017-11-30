library(mlr)


testF = function(data,instance,job){
  data(iris)
  learner = makeImputeWrapper(learner = makeLearner("regr.rpart"),
                              classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), #Not so smart
                                             factor = imputeConstant("NA"), character = imputeConstant("NA")))
  nrOfReps = 3
  
  allInst = list()
  for(i in 1:nrOfReps){
    task = makeRegrTask(data = iris[1:10,], target = "Petal.Width")
    model = train(learner, task)
    allInst[[i]] = model
    #print(str(allInst[[i]]$onlineScenario$aslibScenario$desc$scenario_id))
  } 
  
  
 
  
  print("about to parallelStart")
  myRes = parallelStart(mode="multicore", logging = TRUE, show.info=TRUE, suppress.local.errors=FALSE, cpus=20 ) #Uncomment to run multicore parallelMap (and comment the code above for parallelSocket)
  print("Pre parallelMap")
  myRes = parallelMap(fun = testHelpF,allInst)
  print("Done with parallelMap")
  
  parallelStop()
  
  return(myRes)
}

testHelpF = function(model){
  print("pre prediction")
  prediction = predict(model,newdata=  iris[11:20,])
  print("post prediction")
  return(prediction)
  
}



