## ISLR Ch 8 Ex 12
library(MASS)
library(randomForest)

set.seed(1)

train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train, "crim"]

## Bagging

## 500 trees
bag.boston500 <- randomForest(crim ~ ., data = Boston, subset = train, 
                              mtry = 13,  importance = T)

yhat.bag500 <- predict(bag.boston500, newdata = Boston[-train,])
plot(yhat.bag500, boston.test, main = "Bagging with 500 trees")
abline(0,1)
mean((yhat.bag500 - boston.test)^2)

## 50 trees
bag.boston50 <- randomForest(crim ~ ., data = Boston, subset = train, 
                             mtry = 13, ntree = 50, importance = T)

yhat.bag50 <- predict(bag.boston50, newdata = Boston[-train,])
plot(yhat.bag50, boston.test, main = "Bagging with 50 trees")
abline(0,1)
mean((yhat.bag50 - boston.test)^2)

## 150 trees
bag.boston150 <- randomForest(crim ~ ., data = Boston, subset = train, 
                             mtry = 13, ntree = 150, importance = T)

yhat.bag150 <- predict(bag.boston150, newdata = Boston[-train,])
plot(yhat.bag150, boston.test, main = "Bagging with 150 trees")
abline(0,1)
mean((yhat.bag150 - boston.test)^2)

## Boosting
library(gbm)
par(las = 1)
boost.boston <- gbm(crim ~ ., data = Boston[train,], 
                    distribution = "gaussian", 
                    n.trees = 5000, 
                    interaction.depth = 4)
summary(boost.boston)

par(mfrow = c(1,2))
plot(boost.boston, i = "dis")
plot(boost.boston, i = "medv")

yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

boost.boston <- gbm(medv ~ ., data = Boston[train,], 
                    distribution = "gaussian", n.trees = 5000, 
                    interaction.depth = 4, shrinkage = 0.01, verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)











