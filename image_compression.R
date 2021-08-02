library(png)
x=readPNG('image.png')

grid::grid.raster(x)
dim(x)
svd.r=svd(x)

x1=x[1:512,1:442,1]
svd.r=svd(x1)
grid::grid.raster(x1)
grid::grid.raster(x)
grid::grid.raster(x1)

u1=as.matrix(svd.r$u[-1,1])
dim(u1)

v1=as.matrix(svd.r$v[-1,1])
dim(v1)

d1=diag(svd.r$v[1:1])
dim(d1)

d1=as.matrix(svd.r$v[1:1])
dim(d1)

i1= u1 %*% d1 %*% t(v1)
image(i1)

u5=as.matrix(svd.r$u[,1:5])
v5=as.matrix(svd.r$v[,1:5])
dim(u5)
dim(v5)

d1=diag(svd.r$d[1:1])
dim(d1)

d1=as.matrix(svd.r$d[1:1])
dim(d1)

i1= u1 %*% d1 %*% t(v1)
image(i1)

d5=diag(svd.r$d[1:5])
dim(d5)

i5= u5 %*% d5 %*% t(v5)
image(i5)

u20=as.matrix(svd.r$u[,1:20])
dim(u20)

v20=as.matrix(svd.r$v[,1:20])
dim(v20)

d20=diag(svd.r$d[1:20])
dim(d20)

i20= u20 %*% d20 %*% t(v20)
image(i20)

