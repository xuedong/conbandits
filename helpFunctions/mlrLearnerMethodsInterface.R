#Creates a random forest learner as defined in the mlr-package
#Creates an input wrapper to ensure the method is capable of handling missing values
#The standard amount of decision trees is used
getRandomForestLearner = function(predictionType){
  #Input wrapper is used to be able to handle aslib scenarios with missing data values. 
  #' @references
  #' Ding, Yufeng, and Jeffrey S. Simonoff. An investigation of missing data methods for
  #' classification trees applied to binary response data.
  #' Journal of Machine Learning Research 11.Jan (2010): 131-170.


	learner = makeImputeWrapper(learner = makeLearner("regr.randomForest",predict.type=predictionType), 
	                            classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
	                                           factor = imputeConstant("NA"), character = imputeConstant("NA")))
	
	                            #classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))

#' @references
#' Ding, Yufeng, and Jeffrey S. Simonoff. An investigation of missing data methods for
#' classification trees applied to binary response data.
#' Journal of Machine Learning Research 11.Jan (2010): 131-170.

  return(learner)
}

getRandomForestLearnerJackknife = function(predictionType){
model = makeImputeWrapper(learner = makeLearner("regr.randomForest",predict.type=predictionType, se.method = "jackknife", keep.inbag = TRUE),
                                       classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
                                                      factor = imputeConstant("NA"), character = imputeConstant("NA")))
}



getRangerLearner = function(predictionType){
	learnerRanger = makeImputeWrapper(learner = makeLearner("regr.ranger",predict.type=predictionType), 
	                                  classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
	                                                 factor = imputeConstant("NA"), character = imputeConstant("NA")))


}


getRidgeRegressionLearner = function(predictionType, lambda2 = 1, standardize=TRUE, positive=FALSE, model = c("linear")){
  learnerLR = makeImputeWrapper(learner = makeLearner("regr.penalized.ridge",predict.type=predictionType, lambda2=lambda2, model = model,
                                                      standardize=standardize, positive=positive), 
                                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
                                               factor = imputeConstant("NA"), character = imputeConstant("NA")))
}

getGlmNetLearner = function(predictionType, lambda = 0, family = c("gaussian"), thresh=1e-10,alpha=0, nlambda=1, standardize=FALSE, intercept=FALSE ){
  learnerLR = makeImputeWrapper(learner = makeLearner("regr.glmnet",predict.type=predictionType, thresh=thresh, alpha=alpha, nlambda=nlambda,
                                                      lambda=lambda, standardize=FALSE, intercept=FALSE), 
                                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
                                               factor = imputeConstant("NA"), character = imputeConstant("NA")))
  
}


getLinearRegressionLearner = function(predictionType){
  learnerLR = makeImputeWrapper(learner = makeLearner("regr.lm",predict.type=predictionType), 
                                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(), 
                                               factor = imputeConstant("NA"), character = imputeConstant("NA")))
}
