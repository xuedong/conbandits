require(penalized)
require(glmnet)
require(MASS)

scenarioName = "QBF-2011"
aslibScenario =  parseASScenario(paste(getAslibFilesMapLocation(),aslibScenarioName, sep=""))
nrOfInst = 1000

allAlgNames = getAlgorithmNames(aslibScenario)
algName = allAlgNames[[2]]
instNames = getInstanceNames(aslibScenario)[1:1000]
featCols = c(1, 3:10)
smallFeatureMatrix = aslibScenario$feature.values[aslibScenario$feature.values$instance_id %in% instNames ,featCols]
smallPerfMatrix = aslibScenario$algo.runs[aslibScenario$algo.runs$instance_id %in% instNames 
                                          & aslibScenario$algo.runs$algorithm == algName,c("instance_id","runtime")]

#Sanity check to ensure the oreder of the features and performances correspond 
areSameInst = smallPerfMatrix$instance_id == smallFeatureMatrix$instance_id
if(FALSE %in% areSameInst){
  stop("The instances in the performance matrix are not in the same
       order as those in the feature matrix")
}


myLambda = 1

# compute ridge regression manually 
X = cbind(matrix(1, nrow(smallFeatureMatrix), 1), as.matrix(smallFeatureMatrix[2:ncol(smallFeatureMatrix)]))
Y = as.matrix(smallPerfMatrix[,2])
Z = t(X)%*%X + myLambda*diag(ncol(Y)+ncol(X)-1)

## compute beta using ginv
Ainv= ginv(Z)
b = t(X) %*% Y
beta1 = Ainv %*% b
## compute beta using solve
beta2 = solve(Z, t(X) %*% Y)

# compute ridge regression with penalized
pen = penalized(smallPerfMatrix[,2], penalized=smallFeatureMatrix[2:ncol(smallFeatureMatrix)], positive=FALSE, lambda1=0, lambda2=myLambda)

# compute ridge regression with glmnet
gl <- glmnet(as.matrix(smallFeatureMatrix[2:ncol(smallFeatureMatrix)]), as.matrix(smallPerfMatrix[,2]), standardize=FALSE, family="gaussian", alpha=0, lambda=c(myLambda))


print("ginv:")
print(beta1)
print("solve:")
print(beta2)
print("penalized:")
print(coefficients(pen))
print("glmnet:")
print(coef(gl))

