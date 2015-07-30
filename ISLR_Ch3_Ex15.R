#ISLR Ch3 Ex 15

# load the data
library(MASS)
library(ISLR)

attach(Boston)
# using stargazer for pretty summaries
library(stargazer)

# part a
lm.zn <- lm(crim ~ zn)
stargazer(lm.zn, type = "text")
plot(zn, crim, col= "deeppink3")
abline(lm.zn)

lm.indus <- lm(crim ~ indus)
stargazer(lm.indus, type = "text")
plot(indus, crim, col= "deeppink3")
abline(lm.indus)

lm.chas <- lm(crim ~ chas)
stargazer(lm.chas, type = "text")

lm.nox <- lm(crim ~ nox)
stargazer(lm.nox, type = "text")
plot(nox, crim, col= "deeppink3", 
     xlab = "Nitrogen oxides concentration in Boston", 
     ylab = "Crime rate in Boston")
abline(lm.nox)

lm.rm <- lm(crim ~ rm)
stargazer(lm.rm, type = "text")
plot(rm, crim, col= "dodgerblue3", 
     xlab = "Rooms per dwelling", 
     ylab = "Crime rate in Boston")
abline(lm.rm)

lm.age <- lm(crim ~ age)
stargazer(lm.age, type = "text")
plot(age, crim, col= "darkgreen", 
     xlab = "Owner-occupied units built prior to 1940", 
     ylab = "Crime rate in Boston")
abline(lm.age)

lm.dis <- lm(crim ~ dis)
stargazer(lm.dis, type = "text")

lm.rad <- lm(crim ~ rad)
stargazer(lm.rad, type = "text")

lm.tax <- lm(crim ~ tax)
stargazer(lm.tax, type = "text")

lm.ptratio <- lm(crim ~ ptratio)
stargazer(lm.ptratio, type = "text")
plot(ptratio, crim, col= "darkorchid4", 
     xlab = "Pupil-teacher ratio in Boston", 
     ylab = "Crime rate in Boston")
abline(lm.ptratio)

lm.black <- lm(crim ~ black)
stargazer(lm.black, type = "text")

lm.lstat <- lm(crim ~ lstat)
stargazer(lm.lstat, type = "text")

lm.medv <- lm(crim ~ medv)
stargazer(lm.medv, type = "text")
plot(medv, crim, col= "darkorchid4", 
     xlab = "Median value of owner-occupied homes", 
     ylab = "Crime rate in Boston")
abline(lm.medv)

## part b
lm.all <- lm(crim ~ ., data = Boston)
stargazer(lm.all, type = "text")

# get all the univariate coefficients
u_coefs <- lm.zn$coefficients[2]
u_coefs <- append(u_coefs, lm.indus$coefficients[2])
u_coefs <- append(u_coefs, lm.chas$coefficients[2])
u_coefs <- append(u_coefs, lm.nox$coefficients[2])
u_coefs <- append(u_coefs, lm.rm$coefficients[2])
u_coefs <- append(u_coefs, lm.age$coefficients[2])
u_coefs <- append(u_coefs, lm.dis$coefficients[2])
u_coefs <- append(u_coefs, lm.rad$coefficients[2])
u_coefs <- append(u_coefs, lm.tax$coefficients[2])
u_coefs <- append(u_coefs, lm.ptratio$coefficients[2])
u_coefs <- append(u_coefs, lm.black$coefficients[2])
u_coefs <- append(u_coefs, lm.lstat$coefficients[2])
u_coefs <- append(u_coefs, lm.medv$coefficients[2])

# get the multiple regression coefficients
m_coefs <- lm.all$coefficients[2:14]
plot(u_coefs, m_coefs, xlab= " Univariate coefficients", 
     ylab= "Multiple regression coefficients", col = "darkorange3", 
     main= "Comparison of univariate regression coefficients\nand multiple regression coefficients")

# is there any evidence
# of non-linear association??
stargazer(lm(crim ~ zn + I(zn^2) + I(zn^3)), type = "text")
stargazer(lm(crim ~ indus + I(indus^2) + I(indus^3)), type = "text")
stargazer(lm(crim ~ chas + I(chas^2) + I(chas^3)), type = "text")
stargazer(lm(crim ~ nox + I(nox^2) + I(nox^3)), type = "text")
stargazer(lm(crim ~ rm + I(rm^2) + I(rm^3)), type = "text")
stargazer(lm(crim ~ age + I(age^2) + I(age^3)), type = "text")
stargazer(lm(crim ~ dis + I(dis^2) + I(dis^3)), type = "text")
stargazer(lm(crim ~ rad + I(rad^2) + I(rad^3)), type = "text")
stargazer(lm(crim ~ tax + I(tax^2) + I(tax^3)), type = "text")
stargazer(lm(crim ~ ptratio + I(ptratio^2) + I(ptratio^3)), type = "text")
stargazer(lm(crim ~ black + I(black^2) + I(black^3)), type = "text")
stargazer(lm(crim ~ lstat + I(lstat^2) + I(lstat^3)), type = "text")
stargazer(lm(crim ~ medv + I(medv^2) + I(medv^3)), type = "text")




