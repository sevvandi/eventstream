extract_events_3d <- function(dat,flag="N", filename="nothing", thres, vis=TRUE, tt, epsilon, miniPts){
  # Cluster Data
  output <- get_clusters_3d(dat, flag, filename, thres, vis, epsilon, miniPts)
  cluster.all <- output$clusters
  xyz.high <- output$data
  if(dim(output$data)[1] >0){
    ll <- ceiling(dim(dat)[1]/5)
    normal.stats <- stats_3d(dat[1:ll,,])

        # Compute features
    all.basic.features.1 <- get_features_3d(xyz.high, cluster.all$cluster , normal.stats, dim(dat)[1], tt)
  }else{
    all.basic.features.1 <- NULL
  }

  return(all.basic.features.1)
}

#'Computes event-features
#'
#'This function computes event features of 3D events.
#'
#'@param dat.xyz The data in a cluster friendly format. The first three columns have \code{t},\code{x} and \code{y} positions with the fourth column having the pixel value of that position.
#'@param res.cluster Cluster details from \code{dbscan}.
#'@param normal.stats The background statistics, output from \code{\link{stats_3d}}.
#'@param win_size The window length of the moving window model.
#'@inherit get_features
#'@return
#'An \code{Nx22x4} array  is returned. Here \code{N} is the total number of  events extracted in all windows. The second dimension has \code{30} features and the class label for the \code{supervised} setting.  The third dimension has \code{4} different event ages : \code{tt, 2tt, 3tt, 4tt}.
#'For example, the element at \code{[10,6,3]} has the 6th feature,   of the 10th extracted event when the age of the event is \code{3tt}. The features are listed below:
#'   \item{\code{cluster_id}}{An identification number for each event.}
#'   \item{\code{pixels}}{The number of pixels of each event.}
#'   \item{\code{length}}{The length of the event.}
#'   \item{\code{width}}{The width of the event.}
#'   \item{\code{total_value}}{The total value of the pixels.}
#'   \item{\code{l2w_ratio}}{Length to width ratio of event.}
#'   \item{\code{centroid_x}}{x coordinate of event centroid.}
#'   \item{\code{centroid_y}}{y coordinate of event centroid.}
#'   \item{\code{centroid_z}}{z coordinate of event centroid.}
#'   \item{\code{mean}}{Mean value of event pixels.}
#'   \item{\code{std_dev}}{Standard deviation of event pixels.}
#'   \item{\code{slope}}{Slope of a linear model fitted to the event.}
#'   \item{\code{quad1}}{First coefficient of a quadratic model fitted to the event.}
#'   \item{\code{quad2}}{Second coefficient of a quadratic model fitted to the event.}
#'   \item{\code{sd_from_mean}}{Let us denote the 80th percentile of the event pixels value by \code{x}. How many  standard deviations is \code{x} is away from the mean? }
#'
#'@examples
#' set.seed(1)
#' arr <- array(rnorm(12000),dim=c(40,25,30))
#' arr[25:33,12:20, 20:23] <- 10
#' # getting events
#' out <- get_clusters_3d(arr, "N", "Nothing", thres=0.985, FALSE)
#' mean_sd <- stats_3d(arr[1:20,1:6,1:8])
#' ftrs <- get_features_3d(out$data, out$cluster$cluster, mean_sd, win_size=40, tt=2 )
#'@export
get_features_3d  <- function(dat.xyz, res.cluster, normal.stats, win_size, tt){
  n=17
  num.clusters <- ifelse(0 %in% res.cluster,(length(unique(res.cluster))-1), length(unique(res.cluster)))

  agg.gr <- aggregate(dat.xyz[,1], by =list(res.cluster), function(x) max(x)-min(x)+1)
  # agg.gr2 <- aggregate(dat.xyz[,2], by =list(res.cluster), function(x) 200-max(x))
  # recs <- which((agg.gr[,1]!=0)&((agg.gr[,2]>=10)|(agg.gr2[,2]<6)))
  recs <- which(agg.gr[,1]!=0)

  feature.vector <- array(NA, dim=c(length(recs),n,4))
  mean.z <- mean(dat.xyz[,4])
  sd.z <- sd(dat.xyz[,4])


  k <-1
  for(i in 1:length(unique(res.cluster))){
    ll <-  unique(res.cluster)[i]
    if(ll!=0){
      this.clust.vals <- dat.xyz[res.cluster==ll,]
      age <- diff(range(this.clust.vals[,1]))+1
      # end.time.gap <- 200-max(this.clust.vals[,2])
      # if( (age >=10) | (end.time.gap<6) ){
        for(jkjk in 1:4){
          start.2 <- min(this.clust.vals[,1])
          end.2 <- start.2 + (1:4)*tt
          #end.2 <- c(end.2, max(this.clust.vals[,1]))
          dat.part <- this.clust.vals[which(this.clust.vals[,1]< end.2[jkjk]),]
          if( !is.null(dim(dat.part)) ){
            feature.vector[k,,jkjk]<- c(ll, unlist(get_features_per_cluster_3d( dat.part, normal.stats ) ),0) # normal.stats.splines
          }
        }
        k <- k+1

      #}

    }
  }
  dimnames(feature.vector)[[2]]<- c("cluster_id",  "pixels","length","width", "height", "total_value", "l2w_ratio", "centroid_x", "centroid_y", "centroid_z", "mean", "std_dev", "slope", "quad1", "quad2", "sd_from_global_mean", "Class")
  return(feature.vector)
}



get_features_per_cluster_3d <- function(dat, normal.stats){  # normal.stats.splines

  # dat gives values of dat.xyz belonging to one cluster
  dat <- as.data.frame(dat)

  mean.z <- normal.stats[[1]]
  sd.z <- normal.stats[[2]]
  features <- list()
  features$num.pixels <- dim(dat)[1]
  features$length <- max(dat[,1])-min(dat[,1])+1
  features$width <- max(dat[,2])-min(dat[,2])+1
  features$height <- max(dat[,3])-min(dat[,3])+1
  features$total.value <- sum(dat[,4])
  features$length.2.width.ratio <-  features$length/features$width
  features$cluster.centroid <- data.frame( mean(dat[,1]),mean(dat[,2]),mean(dat[,3]) )
  features$mean.val <- mean(dat[,4])
  features$sd <- sd(dat[,4])
  agg.vals <- aggregate(dat, by=list(dat[,1]), FUN=mean, na.rm=TRUE)
  if(length(agg.vals[,4])>1){
    mod1 <- lm(agg.vals[,5]~agg.vals[,1])
    features$avg.slope <- mod1$coefficients[2]
  }else{
    features$avg.slope <- 0
  }
  if(length(agg.vals[,5])>2){
    mod2 <- lm(agg.vals[ ,5]~poly(agg.vals[ ,1],2))
    features$quad.1 <- mod2$coefficients[2]
    features$quad.2 <- mod2$coefficients[3]
  }else{
    features$quad.1 <- 0
    features$quad.2 <- 0
  }

  features$num.sd.from.global.mean  <- max((quantile(dat[,3],0.8) - mean.z)/sd.z,0)
  return(features)
}


#'Computes mean and standard deviation
#'
#'
#'This function is used for 3D event extraction and feature computation.
#'@param dat The data array
#'@return A list with following components
#'\item{\code{mean.dat}}{The mean of the data array}
#'\item{\code{sd.dat}}{The standard deviation of the data array}
#'@examples
#' set.seed(1)
#' arr <- array(rnorm(12000),dim=c(40,25,30))
#' arr[25:33,12:20, 20:23] <- 10
#' mean_sd <- stats_3d(arr[1:20,1:6,1:8])
#' mean_sd
#'@export
stats_3d <- function(dat){

  mean.dat <- mean(dat)
  sd.dat <- sd(dat)

  return(list(mean.dat, sd.dat))
}


get_class_labels_3d <- function(features.this.chunk, start, end, All.details){
  # In features.this.chunk Centroid.x and Centroid.Y are interchanged
  cen.x <- features.this.chunk[,8,] + start -1
  cen.y <- features.this.chunk[,7,]
  det.x <- All.details$stream_x
  det.y <- All.details$stream_y
  class.col <- dim(features.this.chunk)[2]

  feat.beg.x <- cen.x - features.this.chunk[,4,]/2
  feat.beg.y <- cen.y - features.this.chunk[,3,]/2
  if(is.null(dim(feat.beg.x))){
    feat.beg.x.mean <- mean(feat.beg.x, na.rm=TRUE)
    feat.beg.y.mean <- mean(feat.beg.y,na.rm=TRUE)
  }else{
    feat.beg.x.mean <- apply(feat.beg.x,1,function(x)mean(x, na.rm=TRUE))
    feat.beg.y.mean <- apply(feat.beg.y,1,function(x)mean(x, na.rm=TRUE))
  }
  for(i in 1:length(feat.beg.x.mean)){
    if((sum(abs(det.x - feat.beg.x.mean[i] )<=5) >0) ){
      obj.num <- which(abs(det.x - feat.beg.x.mean[i] )<=5)
      #which(abs(det.x - feat.beg.x.mean[i] )<=5)

      if(length(obj.num)>1){
        obj.num <- which.min(abs(det.y - feat.beg.y.mean[i] ))
      }
      class <- All.details$class[obj.num]
      if(class=="A"){
        features.this.chunk[i,class.col,] <- 1
      }
      if(class=="B"){
        features.this.chunk[i,class.col,] <- 0
      }
    }
  }

  return(features.this.chunk)
}

#'Extracts events from a three-dimensional data stream
#'
#'This function extracts events from a three-dimensional (2D spatial x 1D time) data stream.
#'@inherit get_clusters
#'@examples
#' set.seed(1)
#' arr <- array(rnorm(12000),dim=c(40,25,30))
#' arr[25:33,12:20, 20:23] <- 10
#' # getting events
#' out <- get_clusters_3d(arr, "N", "Nothing", thres=0.985, FALSE)
#' # plots
#' oldpar <- par(mfrow=c(1,3))
#' plot(out$data[,c(1,2)], xlab="x", ylab="y", col=as.factor(out$clusters$cluster))
#' plot(out$data[,c(1,3)], xlab="x", ylab="z",col=as.factor(out$clusters$cluster))
#' plot(out$data[,c(2,3)], xlab="y", ylab="z",col=as.factor(out$clusters$cluster))
#' on.exit(par(oldpar)) 

#'@export
get_clusters_3d <- function(dat, flag, filename="Nothing", thres=0.95, vis=FALSE, epsilon = 3, miniPts = 15){
  events <- get_clusters_3d_2(dat, flag, filename="Nothing", thres=thres, vis=vis, epsilon = epsilon, miniPts = miniPts)
  dat_avg_1 <- apply(dat, 1:2, mean)  
  dat_avg_2 <- apply(dat, 2:3, mean)

  num_pca <- 1
  pc_dat1 <- prcomp(dat_avg_1, scale=FALSE, center = TRUE)
  chg_pts1 <- c()
  for(i in 1:num_pca){
    y <- pc_dat1$x[,i]
    ansvar1=changepoint::cpt.meanvar(y, method="PELT")
    chg_pts1 <- unique(c(chg_pts1, changepoint::cpts(ansvar1) ))
  }
  
  pc_dat2 <- prcomp(t(dat_avg_1), scale=FALSE, center = TRUE)
  chg_pts2 <- c()
  for(i in 1:num_pca){
    y <- pc_dat2$x[,i]
    ansvar2=changepoint::cpt.meanvar(y, method="PELT")
    chg_pts2 <- unique(c(chg_pts2, changepoint::cpts(ansvar2) ))
  }
  pc_dat3 <- prcomp(t(dat_avg_2), scale=FALSE, center = TRUE)
  chg_pts3 <- c()
  for(i in 1:num_pca){
    y <- pc_dat3$x[,i]
    ansvar3=changepoint::cpt.meanvar(y, method="PELT")
    chg_pts3 <- unique(c(chg_pts3, changepoint::cpts(ansvar3) ))
  }
  
  clusts <- unique(events$clusters[[1]])
  clust_chng <- c()
  
  for(kk in 1:length(clusts)){
    if(clusts[[kk]]!=0){
      inds1 <- which(events$clusters[[1]] == clusts[[kk]])
      if(length(chg_pts1) >0){
        for(jj in 1:length(chg_pts1)){
          condition <- ( min(abs(events$data[inds1,1] - chg_pts1[jj]))<5 ) 
          if(condition){
            clust_chng <- c(clust_chng, clusts[[kk]])
          }
        }
      }
      if(length(chg_pts2) >0){
        for(jj in 1:length(chg_pts2)){
          condition <- ( min(abs(events$data[inds1,2] - chg_pts2[jj]))<5 ) 
          if(condition){
            clust_chng <- c(clust_chng, clusts[[kk]])
          }
        }
      }
      if(length(chg_pts3) >0){
        for(jj in 1:length(chg_pts3)){
          condition <- ( min(abs(events$data[inds1,3] - chg_pts3[jj]))<5 ) 
          if(condition){
            clust_chng <- c(clust_chng, clusts[[kk]])
          }
        }
      }
      
    }
  }
  clust_chng <- unique(clust_chng)
  inds2 <- which(events$clusters$cluster %in% clust_chng)
  events2 <- events
  events2$clusters$cluster <- events$clusters$cluster[inds2]
  events2$data <- events$data[inds2 , ]
  colnames(events2$data) <- c("Time","X", "Y", "Value")
  return(events2)
}









get_clusters_3d_2 <- function(dat, flag, filename="Nothing", thres=0.95, vis=FALSE, epsilon = 3, miniPts = 15){
  dat.x <- 1:dim(dat)[1]
  dat.y <- 1:dim(dat)[2]
  dat.z <- 1:dim(dat)[3]
  mesh.xyz <- meshgrid3d(dat.x,dat.y,dat.z)
  xyz.dat <- cbind(mesh.xyz, as.vector(dat) )

  quantile.int <- quantile(dat,probs=thres) # , na.rm=TRUE
  xyz.high <- xyz.dat[xyz.dat[,4] >= quantile.int,]
  xyz.high.xyz <- xyz.high[,1:3]
  res <- dbscan::dbscan(xyz.high.xyz, eps = epsilon, minPts = miniPts) # eps = 3, minPts = 7 # with previous work

  ## Insert to remove clusters at start time - for Moving window
  output <- list()
  xyz.high.xyz.2 <- xyz.high.xyz[res$cluster!=0,]
  res.cluster.2 <- res$cluster[res$cluster!=0]
  if(min(xyz.high.xyz.2[,1]) < 5){
    # a cluster is at the start of the window
    rm.clust <- res.cluster.2[which.min(xyz.high.xyz.2[,1])]
    inds <- res$cluster==rm.clust
    res$cluster[inds] <- 0
    output$clusters <- res
    output$data <- xyz.high

  }else{
    output$clusters <- res
    output$data <- xyz.high
  }
  ## Insert end
  return(output)
}
