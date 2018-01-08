#Creates a random forest learner as defined in the mlr-package
#Creates an input wrapper to ensure the method is capable of handling missing values
#The standard amount of decision trees is used
getRandomForestLearner = function(){
  #Input wrapper is used to be able to handle aslib scenarios with missing data values. 
  #' @references
  #' Ding, Yufeng, and Jeffrey S. Simonoff. An investigation of missing data methods for
  #' classification trees applied to binary response data.
  #' Journal of Machine Learning Research 11.Jan (2010): 131-170.


	learner = makeImputeWrapper(learner = makeLearner("regr.randomForest",predict.type="response"), classes = list(numeric = imputeMax(2), 
	factor = imputeConstant("__miss__")))

#' @references
#' Ding, Yufeng, and Jeffrey S. Simonoff. An investigation of missing data methods for
#' classification trees applied to binary response data.
#' Journal of Machine Learning Research 11.Jan (2010): 131-170.

  return(learner)
}

getRandomForestLearnerJackknife = function(){
model = makeImputeWrapper(learner = makeLearner("regr.randomForest",predict.type="se", se.method = "jackknife", keep.inbag = TRUE),
                                       classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
                                                      factor = imputeConstant("NA"), character = imputeConstant("NA")))
}


getRangerLearner = function(){
	learnerRanger = makeImputeWrapper(learner = makeLearner("regr.ranger",predict.type="response"), 
                                  classes = list(numeric = imputeMax(2), 
                                                 factor = imputeConstant("__miss__")))


}