library(dplyr)

## ----------------------------------------------------------------------------
## k-means control loop
wkmeans <- function(data, k, w = NULL, nstart = 10) {
  err <- centers <- cluster_vec <- iterations <- list()
  for (i in 1:nstart) {
    print(paste0("Doing ", i, " of ", nstart))
    ## Pick random points to initialize centroids
    rand <- sample(1:nrow(data), k)
    
    ## Extract cluster centroids 
    clusters <- data[rand,]
    
    ## Run k-means
    wkm <- wkmeans_single(data, k, clusters, w = w)
    
    ## Store error, cluster centers and cluster assignment
    err[[i]] <- wkm$err
    centers[[i]] <- wkm$centers
    cluster_vec[[i]] <- wkm$clusters
    iterations[[i]] <- wkm$iterations
    
  }
  
  min_id <- which.min(err)
  
  result <- list("errors" = err[[min_id]], 
                 "centers" = centers[[min_id]],
                 "clusters" = cluster_vec[[min_id]],
                 "iterations" = iterations[[min_id]])
  return(result)
}

## k-means by hand from
## https://rpubs.com/hasiegler/926806
wkmeans_single <- function(data, k, clusters, w = NULL) {
  ## Vectors to store cluster assignments 
  cluster_vec <- c()
  last_vec <- c(0) ## For comparison
  
  ## Iteration counter
  iter <- 1
  
  ## Stopping condition (set to 1 when clusters are stable)
  stop <- 0
  
  ## Error vector
  sse <- rep(NA, nrow(data))
  
  while (stop == 0) {
    ## Loop through each observation
    for (i in 1:nrow(data)) {
      ## Calculate distance between observation and centroids
      dist <- dist(rbind(data[i,], clusters))
      ## Find centroid with smallest distance
      i_cluster <- which.min(dist[1:k])
      ## Set assignment
      cluster_vec[i] <- i_cluster
      ## Calculate error
      sse[i] <- sum(data[i,] - clusters[i_cluster, ])^2
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
      # print("Using weights")
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
      err = sum(sse)
      
      sizes <- data %>% 
        cbind(cluster_vec) %>% 
        count(cluster_vec) %>% 
        pull(n)
    }
  }
  
  result <- list("err" = err,
                 "sizes" = sizes, 
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
