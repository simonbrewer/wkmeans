set.seed(42)
library(tidyverse)
source("utils.R")

iris2 <- iris %>% 
  select(-Species)

head(iris2)

iris.wkmeans <- wkmeans(data = iris2, k = 3)

plot(iris$Petal.Length, iris$Petal.Width, 
     pch =16, col = iris.wkmeans$clusters)
points(iris.wkmeans$centers$Petal.Length, 
       iris.wkmeans$centers$Petal.Width,
       pch = 10, cex = 2, col = 1:3)

wgts <- rep(1, nrow(iris))
wgts[which.min(iris$Petal.Length)] <- 1e5
wgts[which.max(iris$Petal.Length)] <- 1e5

iris.wkmeans <- wkmeans(data = iris2, k = 3, w = wgts)

plot(iris$Petal.Length, iris$Petal.Width, 
     pch =16, col = iris.wkmeans$clusters)
points(iris.wkmeans$centers$Petal.Length, 
       iris.wkmeans$centers$Petal.Width,
       pch = 10, cex = 2, col = 1:3)

