# load required package

library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)

#reading csv file
comp <- read.csv("computers.csv", header = TRUE, sep = ",")
head(comp)

#taking only numerical columns
data <- comp[, 2:6]
head(data)

kmc <- kmeans(data, 2, nstart = 25)
kmc

kmc$cluster
kmc$centers
kmc$totss
kmc$withinss
kmc$tot.withinss
kmc$betweenss
kmc$size

ggplot(data, aes(x = speed, y = price, color = factor(kmc$cluster))) + geom_point()

fviz_nbclust(data, kmeans, method = "wss")

fviz_nbclust(data, kmeans, method = "silhouette")

fviz_cluster(kmc, data = data, ellipse.type = "euclid", star.plot = TRUE,
             repel = TRUE, ggtheme = theme_minimal())