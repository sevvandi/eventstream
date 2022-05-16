smooth_data <- function(x, s1, s2){
  d1 <- ceiling(dim(x)[1]/s1)
  d2 <- ceiling(dim(x)[2]/s2)
  y <- matrix(0, nrow=d1, ncol=d2)
  r.st <- 1
  r.en <- s1
  c.st <- 1
  c.en <- s2
  for(i in 1:d1){
    for(j in 1:d2){
      y[i, j] <- mean(x[r.st:r.en, c.st:c.en])
      c.st <- min(c.st + s2, dim(x)[2])
      c.en <- min(c.en + s2, dim(x)[2])
    }
    c.st <- 1
    c.en <- s2

    r.st <- min(r.st + s1, dim(x)[1])
    r.en <- min(r.en + s1, dim(x)[1])
  }
  return(y)
}



unitize <- function(z) {
    # Mean and SD normalization
    mean.z <- mean(z)
    sd.z <- sd(z)
    if (sd.z == 0)
        return(z)
    (z - mean.z)/sd.z
}


remove_infinite_values <- function(X, cols){
  # X has the features
  # Cols are columns with infinite values
  df <- as.data.frame(X[,cols])
  iqr.vals <- apply(df,2, function(x)IQR(x, na.rm=TRUE))
  for(ii in 1:length(cols)){
    # recs<- which(is.infinite(X[,cols[ii]]))
    tf <- sapply(X[,cols[ii]], simplify = 'matrix', is.infinite)
    recs <- which(tf)
    X[recs,cols[ii]] <- 5*iqr.vals[ii]*sign(X[recs,cols[ii]])
  }
  return(X)
}

meshgrid3d <- function(xx,yy,zz){
  xnum <- length(xx)
  ynum <- length(yy)
  znum <- length(zz)
  out <- matrix(0, nrow=xnum*ynum*znum, ncol=3)
  out[,1] <- rep(xx,ynum*znum)
  out[,2] <- rep(rep(yy,each=xnum),znum)
  out[,3] <- rep(zz,each=xnum*ynum)
  return(out)
}

# Taken from AtmRay R package after getting email from Kurt Hornik
# asking to fix reverse dependencies
meshgrid <- function(a,b){
    return(list( x=outer(b*0,a,FUN="+"), y=outer(b,a*0,FUN="+") ))
}
