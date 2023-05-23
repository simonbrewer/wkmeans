set.seed(42)
library(tidyverse)
source("utils.R")

# We create points around three different points.
grp1 <- rcloud(n = 20, R = 10, x_cent =  5, y_cent = 30)
grp2 <- rcloud(n = 20, R = 10, x_cent = 20, y_cent = 10)
grp3 <- rcloud(n = 20, R = 10, x_cent = 50, y_cent = 50)


dat = data.frame(
  rbind(grp1, grp2, grp3),
  id = 1:60,
  group = rep(1:3, each = 20)
)

set.seed(42)
dat.kmeans = wkmeans(data = dat, k = 3)
plot(dat$x, dat$y, col = dat.kmeans$clusters, pch = 16)
points(dat.kmeans$centers$x, dat.kmeans$centers$y,col = 1:3)

wgts <- rep(1, nrow(dat))
dat.kmeans = wkmeans(data = dat, k = 3, w = wgts)
plot(dat$x, dat$y, col = dat.kmeans$clusters, pch = 16)
points(dat.kmeans$centers$x, dat.kmeans$centers$y,col = 1:3)

wgts[which.min(dat$x)] <- 1e4
wgts[which.max(dat$x)] <- 1e4
wgts[which.min(dat$y)] <- 1e4
dat.kmeans = wkmeans(data = dat, k = 3, w = wgts)
plot(dat$x, dat$y, col = dat.kmeans$clusters, pch = 16)
points(dat.kmeans$centers$x, dat.kmeans$centers$y,col = 1:3, cex = 2)
