library(tidyverse)

## ----------------------------------------------------------------------------
## k-means by hand from
## https://rpubs.com/hasiegler/926806
wkmeans <- function(data, k, w = NULL, pca = FALSE) {
  
  #option for Principal Component Analysis
  if(pca == TRUE){
    data <- princomp(data)
    data <- data$scores %>%
      as.data.frame() %>%
      select(Comp.1, Comp.2)
  }
  
  #randomly select the indices of k rows to use as starting
  #centers of the k clusters
  rand <- sample(1:nrow(data), k)
  
  #data frame with k observations that were randomly selected
  clusters <- data[rand,]
  
  #empty vectors that will contain the cluster assignments for each observation
  cluster_vec <- c()
  last_vec <- c(0)
  
  #iteration counter
  iter <- 1
  
  #algorithm will stop once stop is equal to 1
  stop <- 0
  
  while (stop == 0) {
    
    #loop through each observation
    for (i in 1:nrow(data)) {
      
      #find the euclidean distance of the ith observation to each of the clusters
      dist <- data[i,] %>%
        rbind(clusters) %>%
        dist()
      
      #find which cluster the ith observation has the smallest distance with
      i_cluster <- dist[1:k] %>%
        which.min()
      
      #add the cluster assignment for the ith observation to a vector
      #containing the cluster assignments of all observations
      cluster_vec[i] <- i_cluster
      
    }
    
    #check to see if the cluster assignments have changed at all since
    #the last iteration
    if (all(cluster_vec == last_vec)) {
      stop <-  1
    }
    
    #save the cluster assignments from this iteration to another object
    #so we can check to see if cluster assignments changed
    last_vec <- cluster_vec
    
    #group the observations into their assigned clusters and find the means
    #of all the columns to use as the new cluster centers
    if (is.null(w)) {
      clusters <- data %>%
        cbind(cluster_vec) %>%
        group_by(cluster_vec) %>%
        summarise(across(everything(), ~ mean(.x)))
    } else {
      print("WWW")
      print(w)
      clusters <- data %>%
        cbind(cluster_vec, w) %>%
        group_by(cluster_vec) %>%
        summarise(across(everything(), ~ weighted.mean(.x, w = w))) %>%
        select(-w)
    }
    
    #remove the first column that contains the cluster number
    clusters <- clusters[, -1]
    
    iter <- iter + 1
    
    if (stop == 1) {
      sizes <- data %>% 
        cbind(cluster_vec) %>% 
        count(cluster_vec) %>% 
        pull(n)
      
      if (is.null(w)) {
        clusters <- data %>%
        cbind(cluster_vec) %>%
        group_by(cluster_vec) %>%
        summarise(across(everything(), ~ mean(.x)))
      } else {
        print("WWW")
        print(w)
        clusters <- data %>%
          cbind(cluster_vec, w) %>%
          group_by(cluster_vec) %>%
          summarise(across(everything(), ~ weighted.mean(.x, w = w))) %>%
          select(-w)
      }
      
    }
    
  }
  
  result <- list("sizes" = sizes, 
                 "centers" = clusters,
                 "clusters" = cluster_vec,
                 "iterations" = iter)
  return(result)
}

## ----------------------------------------------------------------------------
## Function to create clouds of points 
## https://anderfernandez.com/en/blog/code-k-means-from-scratch-in-r/
rcloud <- function(n, R, x_cent=0, y_cent=0){
  r = R * sqrt(runif(n))
  theta = runif(n) * 2 * pi
  x = x_cent + r * cos(theta)
  y = y_cent + r * sin(theta)
  
  z = data.frame(x = x, y = y)
  return(z)
}
