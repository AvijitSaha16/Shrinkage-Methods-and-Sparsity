library(mixtools)  #for ellipse

N <- 100 # Number of random samples
#set.seed(123)
# Target parameters for univariate normal distributions
rho <- 0.999
mu1 <- 0; s1 <- 1
mu2 <- 0; s2 <- 1

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),
                2) # Covariance matrix

# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}

library(MASS)
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn1) <- c("bvn1_X1","bvn1_X2")

err <- rnorm(100)

y <- 2*bvn1[,1] + 3*bvn1[,2] + err

model <- lm(y ~ bvn1[,1] + bvn1[,2])

summary(model)

# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 1 implements lasso regression
lass_cv <- cv.glmnet(bvn1, y, alpha = 1, lambda = lambdas,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lass_cv)


# Best cross-validated lambda
lambda_cv <- lass_cv$lambda.min#choosing the minimizing lambda as optimal
# Fit final model, get its sum of squared residuals and multiple R-squared
model_lasso <- glmnet(bvn1, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_lasso <- predict(model_lasso, bvn1)
ssr_lasso <- t(y - y_hat_lasso) %*% (y - y_hat_lasso)
rsq_lasso <- cor(y, y_hat_lasso)^2


# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res <- glmnet(bvn1, y, alpha = 1, lambda = lambdas, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(bvn1), cex = .7)
lambda_cv






# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 0 implements ridge regression
rid_cv <- cv.glmnet(bvn1, y, alpha = 0, lambda = lambdas,
                     standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(rid_cv)


# Best cross-validated lambda
lambda_cv <- rid_cv$lambda.min#choosing the minimizing lambda as optimal
# Fit final model, get its sum of squared residuals and multiple R-squared
model_ridge <- glmnet(bvn1, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_ridge <- predict(model_ridge, bvn1)
ssr_ridge <- t(y - y_hat_ridge) %*% (y - y_hat_ridge)
rsq_lridge <- cor(y, y_hat_ridge)^2


# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res <- glmnet(bvn1, y, alpha = 0, lambda = lambdas, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(bvn1), cex = .7)
lambda_cv