# ISLR Ch 5 Ex 9
# Resampling methods
library(MASS)
attach(Boston)

# a) Population mean of medv
mu_medv <- mean(medv)
mu_medv
# [1] 22.53281

# b) estimate the st err of a)
# divide the std dev by number of observation
# 1 --> # of observations = # of rows
dim(Boston)
# [1] 506  14
# 2 --> divide std dev by the sq rt of # of rows
se_mu_medv <- sd(medv)/sqrt(dim(Boston)[1])
# [1] 0.4088611

# c) estimate se of a) using bootstrap
set.seed(1)
# need a function to use with boot()
boot.fn <- function(data, index) {
  mean_medv = mean(data[index])
  return (mean_medv)
}
boot(medv, boot.fn, 1000)

# d) provide a 95% confidence interval
# for the mean of medv
# compare to :
t.test(Boston$medv)
# 21.72953 23.33608

# using mean and se from bootstrap
mean_medv_conf_int <- c(22.53281 - 2*0.4119374, 22.53281 + 2*0.4119374)
mean_medv_conf_int
# [1] 21.70894 23.35668

# e) provide an estimate of the median of medv
med_medv <- median(medv)
med_medv
# [1] 21.2

# f) estimate st er of e) using bootstrap
boot.fn2 <- function(data, index) {
  med <- median(data[index])
  return(med)
}
boot(medv, boot.fn2, 1000)

# g) an estimate for the 10th percentile of medv
perc_10_medv <- quantile(medv, 0.1)
perc_10_medv

# h) use bootstrap to estimate the st er of g)
boot.fn3 <- function(data, index) {
  perc_10 <- quantile(data[index], 0.1)
  return (perc_10)
}
boot(medv, boot.fn3, 1000)













