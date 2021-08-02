#import required packages
library(cluster)
library(factoextra)

#read inbuilt data
data("multishapes")
df <- multishapes[, 1:2]
df

plot(df$x, df$y)

km <- kmeans(df, 5, nstart = 25)
km

fviz_cluster(km, df, geom = "point", ellipse = TRUE,
             palette = "jco", ggtheme = theme_classic())

library(dbscan)
kNNdistplot(df, k =5)
abline(h = 0.15, lty = 2)

library(fpc)
db <- dbscan(df, eps = 0.15, MinPts = 5)
db

fviz_cluster(db, df, geom = "point", ellipse = FALSE,
             palette = "jco", ggtheme = theme_classic())