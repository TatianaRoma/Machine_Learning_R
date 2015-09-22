## ISLR Chapter 10 Ex 11

# a)
data <- read.csv("Downloads/Ch10Ex11.csv", header=F)
names(data)
str(data)
dim(data)

# b)
names(data)[1:20] <- "healthy"
names(data)[21:40] <- "deceased"
scaled_data <- scale(data)
data.dist = as.dist(1 - cor(data))
par(mfrow = c(1,1))
plot(hclust(dd, method="complete"),
     labels = names(data),
     main = "Hierarchical clustering with complete linkage",
     xlab = "", sub = "",ylab = "")

plot(hclust(dd, method="single"),
     labels = names(data),
     main = "Hierarchical clustering with single linkage",
     xlab = "", sub = "",ylab = "")

plot(hclust(dd, method="average"),
     labels = names(data),
     main = "Hierarchical clustering with average linkage",
     xlab = "", sub = "",ylab = "")

# c)
# transpose the table, so genes are columns
trans_data <- t(data)
pca.out2 = prcomp(trans_data)
summary(pca.out2)

loadings <- pca.out2$rotation
rownames(loadings) <- colnames(trans_data)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]
total_load[indices[1:10]]
