# ISLR Chapter 4 Ex 13

library(MASS)
attach(Boston)
library(class)
library(stargazer) # for pretty views

# add new binary variable crimbin
# where 0 is below median(crim)
# and 1 is above or equals to median(crim)
# standardize the predictors 

# 1 - Copy all but crime rate
standBoston <- Boston[,-1]
# 2 - add binary crime rate var
standBoston$crimbin <- 
  ifelse(Boston$crim >= median(Boston$crim), 1, 0)
# 3 - standardize the other predictors
standBoston[, 1:13] <- scale(standBoston[, 1:13])

# Train and test
test <- 1:100
trainBoston <- standBoston[-test,]
testBoston <- standBoston[test,]
trainCrim <- standBoston$crimbin[-test]
testCrim <- standBoston$crimbin[test]

set.seed(1)

# Logistic regression
lr.fit <- glm(crimbin ~ ., data = trainBoston, 
              family = binomial)
stargazer(lr.fit)
lr.probs <- predict(lr.fit, testBoston, 
                    type = "response")

lr.pred <- rep(0, 100)
lr.pred[lr.probs > .5] = 1
table(lr.pred, testCrim)
mean(lr.pred != testCrim)

lr.pred <- rep(0, 100)
lr.pred[lr.probs > .75] = 1
table(lr.pred, testCrim)
mean(lr.pred != testCrim)

lr.pred <- rep(0, 100)
lr.pred[lr.probs > .25] = 1
table(lr.pred, testCrim)
mean(lr.pred != testCrim)


# LDA

# All predictors
lda.fit <- lda(crimbin ~ ., data = trainBoston)
summary(lda.fit)
plot(lda.fit)

lda.pred <- predict(lda.fit, testBoston)
lda.class <- lda.pred$class
table(lda.class, testCrim)
mean(lda.class != testCrim)

# Some predictors
lda.fit2 <- lda(crimbin ~ 
                  nox + dis + rad + tax + ptratio + medv, 
                data = trainBoston)
lda.fit2
plot(lda.fit2)

lda.pred2 <- predict(lda.fit2, testBoston)
lda.class2 <- lda.pred2$class
table(lda.class2, testCrim)
mean(lda.class2 != testCrim)

# KNN
# All predictors
knn.pred <- knn(trainBoston, testBoston, trainCrim, k = 1)
table(knn.pred, testCrim)
mean(knn.pred == testCrim)

# trying with another train and test sets
test2 <- 100:200
trainBoston2 <- standBoston[-test2,]
testBoston2 <- standBoston[test2,]
trainCrim2 <- standBoston$crimbin[-test2]
testCrim2 <- standBoston$crimbin[test2]

knn.pred2 <- knn(trainBoston2, testBoston2, 
                 trainCrim2, k = 1)
table(knn.pred2, testCrim2)
mean(knn.pred2 == testCrim2)

knn.pred2 <- knn(trainBoston2, testBoston2, 
                 trainCrim2, k = 3)
table(knn.pred2, testCrim2)
mean(knn.pred2 == testCrim2)

knn.pred2 <- knn(trainBoston2, testBoston2, 
                 trainCrim2, k = 10)
table(knn.pred2, testCrim2)
mean(knn.pred2 == testCrim2)

knn.pred2 <- knn(trainBoston2, testBoston2, 
                 trainCrim2, k = 20)
table(knn.pred2, testCrim2)
mean(knn.pred2 == testCrim2)


