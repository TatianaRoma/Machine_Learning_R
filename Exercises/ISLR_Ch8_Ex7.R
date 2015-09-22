## ISLR Ch8 Ex7

library(MASS)
library(randomForest)
Boston <- scale(Boston)
set.seed(1)

train <- sample(1:nrow(Boston), nrow(Boston)/2)
crim.test <- Boston[-train, "crim"]
x=matrix(rep(NA,length(mtry)*length(ntree)),length(ntree),length(mtry))
set.seed(1)
mtry=c(2,4,6,8,10)
ntree=c(50,75,100,125,150)

errors <- matrix(data = NA, length(ntree), length(mtry))

for (i in 1:length(ntree)) {
  for (j in 1:length(mtry)) {
    rf.boston <- randomForest(crim ~ ., data = Boston, 
                          subset = train, mtry = mtry[j], 
                          ntree = ntree[i], importance = T)
    y = predict(rf.boston, newdata=Boston[-train,])
    errors[i,j] = mean((y - crim.test)^2)
  }
}
colors=c(1, 2, 3, 4, 5)

plot(ntree, errors[,1], xlab="Number of Trees", ylim=c(.4, .6), 
     ylab="Test Classification Error", col=colors[1], type='l',
     main = "Predicting crime rate in Boston\nwith Random Forests")


for(j in 2:length(mtry)) {
  lines(ntree, errors[,j], col = colors[j])
}

legend("topright", sprintf("%g predictors", mtry), lty = 1, col = colors, cex = 0.6)























