library(MASS)
Boston
?Boston

#Rows, columns and what they represent
dim(Boston)
names(Boston)

# Pairwise scatterplots:
attach(Boston)
#All pairs
pairs(Boston, col="snow4")
#Some pairs
pairs(~ crim + nox + rm + dis + ptratio + black + lstat, Boston, col="snow4")

plot(crim, zn, col="peachpuff4")
plot(crim, nox, col="indianred4")
plot(crim, age, col="palegreen4")
plot(crim, dis, col="lightsalmon3")
plot(crim, tax, col="coral3")
plot(crim, black, col="cyan4")
plot(crim, medv, col="darkgoldenrod4")
plot(as.factor(rad), age, col="aquamarine")
plot(nox, indus, col = "darkolivegreen4")

summary(crim)

boston_bycrime <- Boston[order(-crim),]
head(boston_bycrime, n=10)

boston_bytax <- Boston[order(-tax),]
head(boston_bytax, n=10)

boston_byptratio <- Boston[order(-ptratio),]
head(boston_byptratio, n=10)

table(chas)

summary(ptratio)

#Summary
install.packages("stargazer")
library(stargazer)
stargazer(mt8rooms, type="text")



