random_data <- rbind(cbind(rnorm(200, 5, 8), rnorm(200, 5, 8)),
                     cbind(rnorm(300, 50, 8), rnorm(300, 50, 8)))
random_data
colnames(random_data) <- c("x", "y")
head(random_data)

fviz_nbclust(random_data, clara, method = "silhouette") + theme_classic()
cl = clara(random_data, 2, samples = 50, pamLike = TRUE)
cl

cl$medoids
cl$clustering

fviz_cluster(cl, palette = c("#00FF00", "#0000FF"),
             ellipse.type = "t", #concentration ellipse
             geom = "point", pointsize = 1,
             repel = TRUE, # avoid label overplotting (slow)
             ggtheme = theme_classic())