#read csv file
df <- read.csv("Age_spend.csv", header = TRUE, sep = ",")
df$age

#import required packages
library(cluster)
library(factoextra)

#distance of age
res.dist <- dist(df$age, method = "euclidean")
res.dist
as.matrix(res.dist)

#perform hierarchical clustering
res.hc <- hclust(res.dist, method = "complete")
res.hc

#plot heirarchical clusetring
plot(res.hc)
fviz_dend(res.hc, cex = 0.7)

kcl <- cutree(res.hc, k = 4)
kcl
table(kcl)

rownames(df)[kcl == 1]
fviz_dend(res.hc, k = 4, #cut in four groups
          cex = 0.8, #label size
          k_colors = c("#2e9F00", "#12AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, #color label by groups
          rect = TRUE #add rectangle around groups
)

height.cl <- cutree(res.hc, h = 25)
height.cl          
table(height.cl)          
rownames(df)[height.cl == 1]
fviz_dend(res.hc, k = 2, #cut in four groups
          cex = 0.8, #label size
          k_colors = c("#2e9F00", "#12AFBB"),
          color_labels_by_k = TRUE, #color label by groups
          rect = TRUE #add rectangle around groups
)


#performing agglomerative clustering
res.ag <- agnes(x = df, stand = TRUE, metric = "euclidean", method = "complete")
res.ag
fviz_dend(res.ag, cex = 0.8)

#performing divisive analysis
res.diana <- diana(x = df, stand = TRUE, metric = "euclidean")
res.diana
fviz_dend(res.diana, cex = 0.8)
