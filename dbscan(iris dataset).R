#read inbuilt data
data("iris")
iris <- as.matrix(iris[, 1:4])

plot(df$Sepal.Length, df$Sepal.Width)

km <- kmeans(iris, 4, nstart = 25)
km


library(dbscan)
kNNdistplot(iris, k =  4)
abline(h = 0.4, lty = 2)

library(fpc)
set.seed(123)
# fpc package
res.fpc <- dbscan(iris, eps = 0.4, MinPts = 4)
# dbscan package
res.db <- dbscan(iris, 0.4, 4)

fviz_cluster(res.fpc, iris, geom = "point")
