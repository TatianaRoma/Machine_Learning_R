## ISLR Ch 10 Ex 10

library(clusterGeneration)

# a)
set.seed(11)
# grd <- genRandomClust(numClust = 3,
#                       numNonNoisy = 45,
#                       numNoisy = 5,
#                       sepVal = .7,
#                       rangeVar = c(1, 100),
#                       clustszind = 1,
#                       clustSizeEq = 20,
#                       outputDatFlag = F)
tmp<-simClustDesign(numClust=3,
                    sepVal=.7,
                    numNonNoisy=50,
                    numNoisy = 0,
                    numReplicate=1,
                    clustszind = 1,
                    clustSizeEq = 20,
                    outputDatFlag = F)
x1 <- tmp$datList$"1"$testJLG3v50nv0out0_1

# b)
pca.out = prcomp(x1, scale = T)

PC12 <- pca.out$x[,1:2]
cluster1 <- pca.out$x["PC1" >= 4.9,1:2]
cluster2 <- subset(PC12, PC12[,1] <= 4.9 & PC12[,1] >= -4.8)
cluster3 <- subset(PC12, PC12[,1] <= -4.8)

plot(cluster2, col = "darkgreen",
     xlab="PC1", ylab="PC2", xlim = c(-8, 8), ylim = c(-4, 4))
points(cluster1, col = "orange")
points(cluster3, col = "dodgerblue")

summary(pca.out)
pca.out$x[,1:2]

# c)
km.out = kmeans(x1, 3, nstart=20)
plot(x1, col = (km.out$cluster + 1), 
     main="K-Means Clustering Results with K=3", 
     xlab="", ylab="", pch=(km.out$cluster + 10), cex=2)
#table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# d)
km.out2 <- kmeans(x1, 2, nstart=20)
km.out2$cluster
plot(x1, col = (km.out2$cluster + 1), 
     main="K-Means Clustering Results with K=2", 
     xlab="", ylab="", pch=(km.out2$cluster + 10), cex=2)

# e)
km.out4 <- kmeans(x1, 4, nstart=20)
km.out4$cluster
plot(x1, col = (km.out4$cluster + 1), 
     main="K-Means Clustering Results with K=4", 
     xlab="", ylab="", pch=(km.out4$cluster + 10), cex=2)

# f)
km.out.pc <- kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out.pc$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
plot(x1, col = (km.out.pc$cluster + 1), 
     main="K-Means Clustering Results with K=3
     \n on the first two principal components", 
     xlab="", ylab="", pch=(km.out.pc$cluster + 10), cex.main=.95)

# g)
km.out.sc = kmeans(scale(x1), 3, nstart=20)
km.out.sc$cluster
plot(x1, col = (km.out.sc$cluster + 1), 
     main="K-Means Clustering Results with K=3
     \n after scaling the predictors", 
     xlab="", ylab="", pch=(km.out.sc$cluster + 10), cex.main=.95)






