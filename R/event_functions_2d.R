extract_events <- function(dat,flag="N", filename="nothing", thres, vis=TRUE, tt=10, epsilon=5, miniPts=10){
  # Cluster Data
  output <- get_clusters(dat, flag, filename, thres, vis, epsilon, miniPts)
  cluster.all <- output$clusters
  xyz.high <- output$data
  ll <-  max( ceiling(dim(dat)[1]/5), min( 20, dim(dat)[1] ) )
  normal.stats.splines <- spline_stats(dat[1:ll,])


  # Compute features
  all.basic.features.1 <- get_features(xyz.high, cluster.all$cluster, normal.stats.splines, dim(dat)[1], tt)
  return(all.basic.features.1)
}

#'Computes event-features
#'
#'This function computes event features of 2D events.
#'
#'@param dat.xyz The data in a cluster friendly format. The first two columns have \code{y} and \code{x} positions with the third column having the pixel value of that position.
#'@param res.cluster Cluster details from \code{dbscan}.
#'@param normal.stats.splines The background statistics, output from \code{\link{spline_stats}}.
#'@inherit extract_event_ftrs
#'
#'@examples
#' out <- gen_stream(1, sd=15)
#' zz <- as.matrix(out$data)
#' clst <- get_clusters(zz, vis=TRUE)
#' sstats <- spline_stats(zz[1:100,])
#' ftrs <- get_features(clst$data, clst$clusters$cluster, sstats)

#'@export
get_features  <- function(dat.xyz, res.cluster, normal.stats.splines, win_size=200, tt=10){
  n= 23# 31 # 14
  num.clusters <- ifelse(0 %in% res.cluster,(length(unique(res.cluster))-1), length(unique(res.cluster)))

  agg.gr <- aggregate(dat.xyz[,2], by =list(res.cluster), function(x) max(x)-min(x))
  agg.gr2 <- aggregate(dat.xyz[,2], by =list(res.cluster), function(x) win_size-max(x))
  recs <- which((agg.gr[,1]!=0)&((agg.gr[,2]>=10)|(agg.gr2[,2]<6)))

  feature.vector <- array(NA, dim=c(length(recs),n,4))
  mean.z <- mean(dat.xyz[,3])
  sd.z <- sd(dat.xyz[,3])


  k <-1
  for(i in 1:length(unique(res.cluster))){
    ll <-  unique(res.cluster)[i]
    if(ll!=0){
      this.clust.vals <- dat.xyz[res.cluster==ll,]
      age <- diff(range(this.clust.vals[,2]))
      end.time.gap <- win_size-max(this.clust.vals[,2])
      if( (age >=10) | (end.time.gap<6) ){
        for(jkjk in 1:4){
          start.2 <- min(this.clust.vals[,2])
          end.2 <- start.2 + (1:3)*tt
          end.2 <- c(end.2, max(this.clust.vals[,2]))
          dat.part <- this.clust.vals[which(this.clust.vals[,2]< end.2[jkjk]),]
          if( !is.null(dim(dat.part)) ){
            if( dim(dat.part)[1] >0 ){
              feature.vector[k,,jkjk]<- c(ll, unlist(get_features_per_cluster(dat.part, normal.stats.splines)),0)
            }
          }
        }
        k <- k+1

      }

    }
  }
  dimnames(feature.vector)[[2]]<- c("cluster_id",  "pixels","length","width", "total_value", "l2w_ratio", "centroid_x", "centroid_y", "mean", "std_dev", "avg_slope","quad_1","quad_2", "2sd_from_mean", "3sd_from_mean", "4sd_from_mean",  "5iqr_from_median","6iqr_from_median","7iqr_from_median","8iqr_from_median", "iqr_from_median", "sd_from_global_mean", "Class")
  return(feature.vector)
}



get_features_per_cluster <- function(dat,normal.stats.splines){
  ##mean.z,sd.z, med.spline, iqr.spline, mean.spline, sd.spline
  # dat gives values of dat.xyz belonging to one cluster
  dat <- as.data.frame(dat)
  med.spline <- normal.stats.splines[[1]]
  iqr.spline <- normal.stats.splines[[2]]
  mean.spline <- normal.stats.splines[[3]]
  sd.spline <- normal.stats.splines[[4]]
  mean.z <- normal.stats.splines[[5]]
  sd.z <- normal.stats.splines[[6]]
  features <- list()
  features$num.pixels <- dim(dat)[1]
  features$length <- max(dat[,1])-min(dat[,1])+1
  features$width <- max(dat[,2])-min(dat[,2])+1
  features$total.value <- sum(dat[,3])
  features$length.2.width.ratio <-  features$length/features$width
  features$cluster.centroid <- data.frame(mean(dat[,1]),mean(dat[,2]))
  features$mean.val <- mean(dat[,3])
  features$sd <- sd(dat[,3])
  agg.vals <- aggregate(dat, by=list(dat[,1]), FUN=mean, na.rm=TRUE)
  mod1 <- lm(agg.vals[,3]~agg.vals[,1])
  features$avg.slope <- mod1$coefficients[2]
  if(length(agg.vals[,3])>2){
    mod2 <- lm(agg.vals[,3]~poly(agg.vals[,1],2))
    features$quad.1 <- mod2$coefficients[2]
    features$quad.2 <- mod2$coefficients[3]
  }else{
    features$quad.1 <- mod1$coefficients[2]
    features$quad.2 <- 0
  }
  median.pred <- predict(med.spline, features$cluster.centroid[,1])
  median.z <- median.pred$y
  iqr.pred <- predict(iqr.spline, features$cluster.centroid[,1])
  iqr.z <- iqr.pred$y
  mean.pred <-  predict(mean.spline, features$cluster.centroid[,1])
  mean.spl.z <- mean.pred$y
  sd.pred <-  predict(sd.spline, features$cluster.centroid[,1])
  sd.spl.z <- sd.pred$y
  features$percentage.g.sd.2 <- sum(dat[,3] > (mean.z + 2*sd.z))/length(dat[,3])
  features$percentage.g.sd.3 <- sum(dat[,3] > (mean.z + 3*sd.z))/length(dat[,3])
  features$percentage.g.sd.4 <- sum(dat[,3] > (mean.z + 4*sd.z))/length(dat[,3])
  features$percentage.g.iqr.5 <- sum(dat[,3] > (median.z + 5*iqr.z))/length(dat[,3])
  features$percentage.g.iqr.6 <- sum(dat[,3] > (median.z + 6*iqr.z))/length(dat[,3])
  features$percentage.g.iqr.7 <- sum(dat[,3] > (median.z + 7*iqr.z))/length(dat[,3])
  features$percentage.g.iqr.8 <- sum(dat[,3] > (median.z + 8*iqr.z))/length(dat[,3])
  features$num.iqr.from.med <- max((quantile(dat[,3],0.75) - median.z)/iqr.z,0)
  features$num.sd.from.global.mean  <- max((quantile(dat[,3],0.8) - mean.z)/sd.z,0)
  return(features)
}


#'Computes background quantities using splines
#'
#'This function computes 4 splines, from median, iqr, mean and standard deviation values.
#'@param dat The data matrix
#'@return A list with following components
#'\item{\code{med.spline}}{The spline computed from the median values.}
#'\item{\code{iqr.spline}}{The spline computed from IQR values.}
#'\item{\code{mean.spline}}{The spline computed from mean values.}
#'\item{\code{sd.spline}}{The spline computed from standard deviation values.}
#'\item{\code{mean.dat}}{The mean of the data matrix.}
#'\item{\code{sd.dat}}{The standard deviation of the data matrix.}
#'
#' @examples
#' out <- gen_stream(1, sd=15)
#' zz <- as.matrix(out$data)
#' sstats <- spline_stats(zz[1:100,])
#' par(mfrow=c(2,1))
#' image(1:ncol(zz), 1:nrow(zz),t(zz), xlab="Location", ylab="Time" )
#' plot(sstats[[1]], type="l")
#'@export

spline_stats <- function(dat){
  dat.col.medians <- apply(dat,2,median)
  dat.col.iqr <- apply(dat,2,IQR)
  dat.col.mean <- apply(dat,2, mean)
  dat.col.sd <- apply(dat,2,sd)
  df1 <- 12
  med.spline <-
    smooth.spline(1:length(dat.col.medians),dat.col.medians, df = df1)
  iqr.spline <- smooth.spline(1:length(dat.col.iqr),dat.col.iqr, df = df1)
  mean.spline <- smooth.spline(1:length(dat.col.mean),dat.col.mean, df = df1)
  sd.spline <- smooth.spline(1:length(dat.col.sd),dat.col.sd, df = df1)
  mean.dat <- mean(as.matrix(dat))
  sd.dat <- sd(as.matrix(dat))
  return(list(med.spline,iqr.spline,mean.spline,sd.spline, mean.dat, sd.dat))
}

get_class_labels <- function(features.this.chunk, start, end, All.details){
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

    x_cond <- abs(det.x - feat.beg.x.mean[i] )<=5
    y_cond <- abs(det.y - feat.beg.y.mean[i] )<=5

    if(sum(x_cond & y_cond)>0){
      obj.num <- which(x_cond & y_cond)

      if(length(obj.num)>1){
        obj.num <- which.min(abs(det.y - feat.beg.y.mean[i]) + abs(det.x - feat.beg.x.mean[i]))
      }

      class <- All.details$class[obj.num]
      if(class=="A"){
        features.this.chunk[i,class.col,] <- 1
      }else{
        features.this.chunk[i,class.col,] <- 0
      }

    }else{
      features.this.chunk[i,class.col,] <- 0
    }

  }

  return(features.this.chunk)
}

#'Extracts events from a two-dimensional data stream
#'
#'This function extracts events from a two-dimensional (1 spatial x 1 time) data stream.
#'@param dat The data matrix
#'@param flag If \code{Y}, then  event images  are saved as jpeg files.
#'@param filename The name of the file of the event images.
#'@inherit extract_event_ftrs
#'@return A list with following components
#'\item{\code{clusters}}{The cluster assignment according to DBSCAN output.}
#'\item{\code{data}}{The data of this cluster assignment.}
#'
#'@examples
#'out <- gen_stream(2, sd=15)
#'zz <- as.matrix(out$data)
#'clst <- get_clusters(zz, vis=TRUE)
#'
#'@export
get_clusters <- function(dat, flag="N", filename="Nothing", thres=0.95, vis=FALSE, epsilon =5, miniPts = 10){
  dat.x <- 1:dim(dat)[2]
  dat.y <- 1:dim(dat)[1]
  mesh.xy <- AtmRay::meshgrid(dat.x,dat.y)
  xyz.dat <- cbind(as.vector(mesh.xy$x), as.vector(mesh.xy$y), as.vector(as.matrix(dat)) )

  quantile.int <- quantile(as.matrix(dat),probs=thres) # , na.rm=TRUE
  xyz.high <- xyz.dat[xyz.dat[,3] > quantile.int,]
  xyz.high.xy <- xyz.high[,1:2]
  res <- dbscan::dbscan(xyz.high.xy, eps = epsilon, minPts = miniPts) # eps = 3, minPts = 7 # with previous work
  if(flag=="Y"){
    jpeg(filename,width = 750, height = 600, quality=100)
    par(pty="s", mfrow=c(1,2))
    dat <- as.matrix(dat)
    image(1:dim(dat)[1], 1:dim(dat)[2], dat, col = topo.colors(100),axes=FALSE, xlab="Dimension 1", ylab="Dimension 2")
    axis(1, at = seq(10, dim(dat)[1], by = 10))
    axis(2, at = seq(50, dim(dat)[2], by = 50))
    title(main = "Original", font.main = 2)
    plot(xyz.high.xy[res$cluster!=0,c(2,1)], col=res$cluster[res$cluster!=0]+1L, pch=19, xlim=c(0,dim(dat)[1]), ylim=c(0,dim(dat)[2]), xlab="Dimension 1", ylab="Dimension 2", main="Events"  )

    dev.off()
  }
  if(vis){
    par(pty="s", mfrow=c(1,2))
    dat <- as.matrix(dat)
    image(1:dim(dat)[1], 1:dim(dat)[2],dat,col = topo.colors(100),axes=FALSE, xlab="Dimension 1", ylab="Dimension 2")
    axis(1, at = seq(10, dim(dat)[1], by = 10))
    axis(2, at = seq(50, dim(dat)[2], by = 50))
    title(main = "Original", font.main = 2)
    plot(xyz.high.xy[res$cluster!=0,c(2,1)], col=res$cluster[res$cluster!=0]+1L, pch=19, xlim=c(0,dim(dat)[1]), ylim=c(0,dim(dat)[2]), xlab="Dimension 1", ylab="Dimension 2", main="Events"  )

  }

  ## Insert to remove clusters at start time - for Moving window
  output <- list()
  xyz.high.xy.2 <- xyz.high.xy[res$cluster!=0,]
  res.cluster.2 <- res$cluster[res$cluster!=0]
  if(min(xyz.high.xy.2[,2]) < 5){
    # a cluster is at the start of the window
    rm.clust <- res.cluster.2[which.min(xyz.high.xy.2[,2])]
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


