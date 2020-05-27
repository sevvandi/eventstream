#'Tunes 2D event detection using labeled data
#'
#'This function finds best parameters for 2D event detection using labeled data.
#'
#'@param x The data in an mxn matrix or dataframe.
#'@param cl The actual locations of the events.
#'@param alpha_min The minimum threshold value.
#'@param alpha_max The maximum threshold value.
#'@param alpha_step The incremental step size for alpha.
#'@param epsilon_min The minimum epsilon value for DBSCAN clustering.
#'@param epsilon_max The maximum epsilon value for DBSCAN clustering.
#'@param epsilon_step The incremental step size for epsilon for DBSCAN clustering.
#'@param minPts_min The minimum minPts value for for DBSCAN clustering.
#'@param minPts_max The maximum minPts value for for DBSCAN clustering.
#'@param minPts_step The incremental step size for minPts for DBSCAN clustering.
#'
#'@return A list with following components
#'\item{\code{best}}{The best threshold, epsilon and MinPts for 2D event detection and the associated Jaccard Index.}
#'\item{\code{all}}{All parameter values used and the associated Jaccard Index values.}
#'
#'@examples
#'out <- gen_stream(1, sd=15)
#'zz <- as.matrix(out$data)
#'clst <- get_clusters(zz, filename = NULL, thres = 0.95, vis = TRUE, epsilon = 5, miniPts = 10, rolling = FALSE)
#'clst_loc <- clst$data[ ,1:2]
#'out <- tune_cpdbee_2D(zz, clst_loc)
#'out$best
#'@export
tune_cpdbee_2D <- function(x, cl, alpha_min = 0.95, alpha_max = 0.98, alpha_step = 0.01, epsilon_min = 2, epsilon_max = 12, epsilon_step = 2, minPts_min = 4, minPts_max = 12, minPts_step = 2){
  # x is the original data
  # x is an mxn matrix
  # cl contains the actual locations of clusters
  # cl has two columns - first column is the x coordinate, second column is the y coordinate
  if(is.null(cl)){
    stop("You need to put in the event locations!")
  }
  alph <- seq(alpha_min, alpha_max, by=alpha_step)
  eps <- seq(epsilon_min, epsilon_max, by=epsilon_step)
  mp <- seq(minPts_min, minPts_max, by =minPts_step)
  tot_rows <- length(alph)*length(eps)*length(mp)
  
  jaccard_inds <- matrix(nrow=tot_rows, ncol=4)
  colnames(jaccard_inds) <- c("Threshold", "Epsilon", "Min_Pts", "Jaccard_Index")
  kk <- 1
  for(aa in alph){
    for(ee in eps){
      for(mm in mp){
        out <- get_clusters(x, filename = NULL, thres = aa, vis = FALSE,
                            epsilon = ee, miniPts = mm, rolling = FALSE)
        jaccard_inds[kk, 1] <- aa
        jaccard_inds[kk, 2] <- ee
        jaccard_inds[kk, 3] <- mm
        
        # Compute Jaccard Index
        output <- as.data.frame(out$data[ ,1:2])
        cl <- as.data.frame(cl)
        match_rows <- dplyr::inner_join(output, cl)
        if(is.null(match_rows)){
          jaccard_inds[kk, 4] <- 0
        }else{
          jaccard_inds[kk, 4] <- dim(match_rows)[1]/(dim(output)[1] + dim(cl)[1] - dim(match_rows)[1])
        }
        kk <- kk +1
      }
    }
  }
  ind <- which.max(jaccard_inds[ ,4])
  obj <- list()
  obj$best <- jaccard_inds[ind, ]
  obj$all <- jaccard_inds
  return(obj)
}

#'Tunes 3D event detection using labeled data
#'
#'This function finds best parameters for 3D event detection using labeled data.
#'@inherit tune_cpdbee_2D
#'
#'@examples
#'set.seed(1)
#'arr <- array(rnorm(12000),dim=c(40,25,30))
#'arr[25:33,12:20, 20:23] <- 10
#'# Getting events
#'out <- get_clusters_3d(arr, thres=0.985) 
#'out <- tune_cpdbee_3D(arr, out$data[ ,1:3])
#'out$best
#'@export
tune_cpdbee_3D <- function(x, cl, alpha_min = 0.95, alpha_max = 0.98, alpha_step = 0.01, epsilon_min = 2, epsilon_max = 12, epsilon_step = 2, minPts_min = 8, minPts_max = 16, minPts_step = 2){
  # x is the original data
  # x is an mxnxl array
  # cl contains the actual locations of clusters
  # cl has two columns - first column is the x coordinate, second column is the y coordinate
  if(is.null(cl)){
    stop("You need to put in the event locations!")
  }
  alph <- seq(alpha_min, alpha_max, by=alpha_step)
  eps <- seq(epsilon_min, epsilon_max, by=epsilon_step)
  mp <- seq(minPts_min, minPts_max, by =minPts_step)
  tot_rows <- length(alph)*length(eps)*length(mp)
  
  jaccard_inds <- matrix(nrow=tot_rows, ncol=4)
  colnames(jaccard_inds) <- c("Threshold", "Epsilon", "Min_Pts", "Jaccard_Index")
  kk <- 1
  for(aa in alph){
    for(ee in eps){
      for(mm in mp){
        out <- get_clusters_3d(x, thres = aa, epsilon = ee, miniPts = mm)
        jaccard_inds[kk, 1] <- aa
        jaccard_inds[kk, 2] <- ee
        jaccard_inds[kk, 3] <- mm
        
        # Compute Jaccard Index
        output <- as.data.frame(out$data[ ,1:3])
        cl <- as.data.frame(cl)
        match_rows <- dplyr::inner_join(output, cl)
        if(is.null(match_rows)){
          jaccard_inds[kk, 4] <- 0
        }else{
          jaccard_inds[kk, 4] <- dim(match_rows)[1]/(dim(output)[1] + dim(cl)[1] - dim(match_rows)[1])
        }
        kk <- kk +1
      }
    }
  }
  ind <- which.max(jaccard_inds[ ,4])
  obj <- list()
  obj$best <- jaccard_inds[ind, ]
  obj$all <- jaccard_inds
  return(obj)
}



calculate_Jaccard_index <- function(x, cl, alpha = 0.95, epsilon = 6, minPts = 10, vis=TRUE){
  # x is the original data
  # x is an mxn matrix
  # cl contains the actual locations of clusters
  # cl has two columns - first column is the x coordinate, second column is the y coordinate
  if(is.null(cl)){
    stop("You need to put in the event locations!")
  }
  out <- get_clusters(x, filename = NULL, thres = alpha, vis = vis,
                      epsilon = epsilon, miniPts = minPts, rolling = FALSE)
  # Compute Jaccard Index
  output <- as.data.frame(out$data[ ,1:2])
  cl <- as.data.frame(cl)
  match_rows <- dplyr::inner_join(output, cl)
  if(is.null(match_rows)){
    jaccard_ind <- 0
  }else{
    jaccard_ind <- dim(match_rows)[1]/(dim(output)[1] + dim(cl)[1] - dim(match_rows)[1])
  }
  return(jaccard_ind)
}

