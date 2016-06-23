library(ISLR)
states <- row.names(USArrests)
names(USArrests)
summary(USArrests) # the mean of all is very different
# to check the variance, we use the apply function
apply(USArrests, 2, var) # the variance is also very different 
# we must standardise the variables before we perform pca as the result will be very erratic otherwise 
# we use the function pcrcomp()

pr.out <- prcomp(USArrests, scale. = T)
# By default, the function centers the variables to a mean of 0 and scale = T makes the standard deviation 1
names(pr.out)
# the center and scale components correspond to the mean and standard deviation of the variables used before the implementation
# of PCA
pr.out$center # mean of the variables before implementation of PCA
# The rotation matrix corresponds to the principal components loading. Each column of pr.out$rotation contains the corresponding 
# principal component loading vector 
pr.out$rotation
# There are 4 principal components. Expected as there are in general a min of (n - 1, p) informative components in a dataset 
# with n observations and p variables. 
# using the prcomp function, we do not need to explicitly multiply the data by principal component loading vectors in order to
# obtain the principal component score vectors. 
# the 50x4 matrix has as its columns the principal component score vectors. 
dim(pr.out$x)

# we can plot the first two principal components 
biplot(pr.out, scale = 0) # scale ensures that the arrows are scaled to represent the loadings 
# the function also gives the standard deviation of each component. 
pr.out$sdev
# variance can be obtained by squaring these. 
pr.var <- pr.out$sdev^2
# We can also compute the proportion of variance explained
pve <- pr.var/sum(pr.var)
pve
# We can plot the variance explained as well as the cumulative variance explained
plot(pve, xlab = "Principal Component", ylab = "Proportion of variance explained", ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = "Principal Component", ylab = "Proportion of variance explained", ylim = c(0, 1), type = "b")
# be aware of how cumsum works.

### Clustering
# K means clustering
# function kmeans performs k means clustering. 
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
# We now perform k means clustering by using k = 2
km.out <- kmeans(x, 2, nstart = 20)
# the cluster assignments are contained in
km.out$cluster
# The clustering perfectly separated the data even though no data was supplied to the function
# The data can be plotted with each observation corresponding to the colour of the cluster
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering with k = 2", xlab = " ", ylab = " ", pch = 20, cex = 2)
# here the observations can be easily plotted as they are 2 dimensional. If there were more than 2 variables, we could
# use PCA and plot the first two 
# Here, we knew that there were 2 clusters as we built the data. In a real scenario, we may go wrong with the number

set.seed(3)
km.out <- kmeans(x, 3, nstart = 20)
km.out
# To run the function with multiple initial cluster assignments, we can use the nstart argument
# If a value of nstart > 1 is used, then K means clustering is performed using multiple random assignments in step 1
# of the algorithm
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss
# km.out$tot.withinss is the total within clusters sum of squares which we seek to minimise by performing k means clustering
# the within cluster sum of squares are contained in the vector km.out$withinss
# ISLR recommends always using k means with a large nstart since, otherwise an undesirable local optimum may be obtained 
# k means output will be reproducible if seed is set.

# Heirarchical clustering
# we use the function hclust and plot the heirarchical clustering dendogram using complete, single and average linkage 
# clustering with euclidean distance as the dissimilarity measure

hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")
# dendograms can be plotted by using the plot function
par(mfrow = c(1, 3))
plot(hc.complete, main = "complete", cex = 0.9)
plot(hc.single, main = "single", cex = 0.9)
plot(hc.average, main = "average", cex = 0.9)

# to determine the for each observations, we use the cutree function
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
# for this data, the function separates quite well for average and complete. 
# however, single identifies only one observation in its own cluster
# A more sensible answer is obtained when 4 clusters are selected.
cutree(hc.single, 4)
# the scale function can be used to scale the variables before performing heirarchical clustering 

xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Heirarchical clustering with scaled features")
# Corelation based features can be computed using the as.dist() function which covers an arbitrary square symmetric matrix 
# into a form that hclust recognises as a distance matrix. this only makes sense for data with at least 3 features 
# since the absolute corelation between any 2 variables will always be 1

# clustering a 3 dimensional data set
x <-matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete linkage with corelation based distance", xlab = "", ylab = "")

### NCI60 Data example
nci.labs <- NCI60$labs
nci.data <- NCI60$data
# We illustrate
# these techniques on the NCI60 cancer cell line microarray data, which
# # consists of 6,830 gene expression measurements on 64 cancer cell lines.
# Each cell line is labeled with a cancer type. We do not make use of the
# cancer types in performing PCA and clustering, as these are unsupervised
# techniques. But after performing PCA and clustering, we will check to
# see the extent to which these cancer types agree with the results of these
# unsupervised techniques.
dim(nci.data)
nci.labs[1:4]
# PCA on the NCI60 data

pr.out <- prcomp(nci.data, scale = T)
Cols=function (vec){
  cols=rainbow (length (unique (vec )))
  return (cols[as.numeric (as.factor (vec))])
}

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col =Cols(nci.labs), pch =19, xlab = "Z1", ylab = "Z3")

summary(pr.out)
# we can also plot the variance explained by the first few principal components

pve <- 100 * pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve ,type = "o",ylab = "PVE", xlab = "Principal Component",col = "blue")
plot(cumsum(pve), type = "o",ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

# CLustering the observations in the data

sd.data <- scale(nci.data)
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist, method = "complete"), labels = nci.labs, main = "Complete linkage",
     xlab = "", ylab = "", sub = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "average linkage",
     xlab = "", ylab = "", sub = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "single linkage",
     xlab = "", ylab = "", sub = "")


# we cut the dendogram at a height which gives a certain number of clusters
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
par(mfrow = c(1, 1))
plot(hc.out,labels = nci.labs)
abline(h = 139, col = "red")
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs,main = "Hier. Clust . on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
