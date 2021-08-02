# loading NBclust package
library(NbClust)

# reading inbuilt USArrests dataset
data("USArrests")
head(USArrests, 5)

# scaling the values and storing it into variables
df <- scale(USArrests)
head(df)

library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")

fviz_nbclust(df, kmeans, method = "silhouette")

fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 50)


nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

