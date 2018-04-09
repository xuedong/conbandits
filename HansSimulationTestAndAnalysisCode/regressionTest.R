require(penalized)
require(glmnet)
require(MASS)

dataCredit <- read.csv("dataCredit.csv", sep=",", header=T)
data <- subset(dataCredit, select=c(Income, Limit, Rating, Cards, Age, Education, Balance))

myLambda = 1

# compute ridge regression manually 
X = cbind(matrix(1, dim(data)[1], 1), as.matrix(data[1:6]))
Y = as.matrix(data$Balance)
Z = t(X)%*%X + myLambda*diag(7)

## compute beta using ginv
Ainv= ginv(Z)
b = t(X) %*% Y
beta1 = Ainv %*% b
## compute beta using solve
beta2 = solve(Z, t(X) %*% Y)

# compute ridge regression with penalized
pen = penalized(data$Balance, penalized=data[1:6], positive=FALSE, lambda1=0, lambda2=myLambda)

# compute ridge regression with glmnet
gl <- glmnet(as.matrix(data[1:6]), as.matrix(data$Balance), standardize=FALSE, family="gaussian", alpha=0, lambda=c(myLambda))


print("ginv:")
print(beta1)
print("solve:")
print(beta2)
print("penalized:")
print(coefficients(pen))
print("glmnet:")
print(coef(gl))
