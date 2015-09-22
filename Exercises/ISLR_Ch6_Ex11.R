## ISLR Ch 6
# Tatiana Romanchishina

# predict per capita crime rate in the Boston data set
library(MASS)
library(glmnet)
library(leaps)
library(pls)

Boston <- na.omit(Boston)

set.seed(1)

# the function from the lab exercises
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}
# Best Subset Selection
k <- 10
p <- dim(Boston)[2]-1
folds <- sample(1:k, nrow(Boston), replace = T)
cv.errors <- matrix(NA, k, p)
for(i in 1:k) {
  best.fit <- regsubsets(crim ~ ., data = Boston[folds != i,], nvmax = p)
  for(j in 1:p) {
    pred <- predict.regsubsets(best.fit, Boston[folds == i,], id = j)
    cv.errors[i, j] <- mean((Boston$crim[folds == i] - pred)^2)
  }
}
mse <- sqrt(apply(cv.errors, 2, mean))
plot(mse, pch = 20, type = "b")

which.min(mse)
# [1] 9
mse[which.min(mse)]
# [1] 6.81526
best.fit <- regsubsets(crim ~ ., data = Boston, nvmax = p)
coef(best.fit, 9)
#   (Intercept)            zn         indus           nox           dis           rad       ptratio         black         lstat          medv 
#  19.124636156   0.042788127  -0.099385948 -10.466490364  -1.002597606   0.539503547  -0.270835584  -0.008003761   0.117805932  -0.180593877


# Prepare for lasso and ridge regression
x <- model.matrix(crim ~ ., Boston)[,-1]
y <- Boston$crim

# prepare train and test sets
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
grid <- 10^seq(10, -2, length = 100)

# Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# find the best lambda
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# [1] 0.4634552

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)
# [1] 48.59338

# fit on the full data set
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef

# Ridge Regression
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)

# find the best lambda
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# [1] 0.5713693
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)
# [1] 47.51344

# refit on the full set
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)

# PCR
set.seed(2)
pcr.fit <- pcr(crim ~ ., data = Boston, subset = train, scale = T, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

# 8 comp-s has the lowest error value
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 8)
mean((pcr.pred - y.test)^2)
# [1] 48.45407