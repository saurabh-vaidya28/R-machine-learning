# load required package
library(subspace)
library(mstknnclust)

data("dslanguages")
print(dslanguages)
str(dslanguages)
dslanguages[,1]

result <- mst.knn(dslanguages)
result$network
result$cluster
result$csize
result$cnumber
result$partition

library(igraph)
#to check vertices of any graph
V(result$network)
plot(result$network, vertex.size = 8, vertex.color = clusters(result$network)$membership)
