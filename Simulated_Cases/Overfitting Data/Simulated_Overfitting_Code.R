library(tidyr)
x1 <- runif(200, -10, 10)
y1 <- runif(200, -10, 10)
z1 <- 2*x1 + 3*y1
err <- rnorm(50)
x2 <- runif(50, -15, 15)
y2 <- runif(50, -15, 15)
z2 <- x2 + 2*y2 + err
X=cbind(union(x1,x2),union(y1,y2))
Y <- union(z1,z2)
Z <- cbind(Y,X)
library("caTools")
ind = sample.split(Z[,1], SplitRatio = 0.8)

#subsetting into Train data
train = Z[ind,]

#subsetting into Test data
test = Z[!ind,]

model1 <- lm(train[,1] ~ train[,2] + train[,3])

summary(model1)
beta=as.vector(model1$coefficients)
D=cbind(as.vector(1),test[,2],test[,3])
A=test[,1]-D%*%beta
E=(t(A)%*%A)/50
E


# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 1 implements lasso regression
lass_cv <- cv.glmnet(X, Y, alpha = 1, lambda = lambdas,
                     standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lass_cv)


# Best cross-validated lambda
lambda_cv <- lass_cv$lambda.min#choosing the minimizing lambda as optimal
# Fit final model, get its sum of squared residuals and multiple R-squared
model_lasso <- glmnet(X, Y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_lasso <- predict(model_lasso, bvn1)
ssr_lasso <- t(Y - y_hat_lasso) %*% (Y - y_hat_lasso)
rsq_lasso <- cor(Y, y_hat_lasso)^2


# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res <- glmnet(X, Y, alpha = 1, lambda = lambdas, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(bvn1), cex = .7)
lambda_cv






# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 0 implements ridge regression
rid_cv <- cv.glmnet(X, Y, alpha = 0, lambda = lambdas,
                    standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(rid_cv)


# Best cross-validated lambda
lambda_cv <- rid_cv$lambda.min#choosing the minimizing lambda as optimal
# Fit final model, get its sum of squared residuals and multiple R-squared
model_ridge <- glmnet(X, Y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_ridge <- predict(model_ridge, bvn1)
ssr_ridge <- t(Y - y_hat_ridge) %*% (Y - y_hat_ridge)
rsq_lridge <- cor(Y, y_hat_ridge)^2


# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res <- glmnet(X, Y, alpha = 0, lambda = lambdas, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(bvn1), cex = .7)
lambda_cv