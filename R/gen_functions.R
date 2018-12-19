create_object_class_A <- function(st.p, leng){

  x.st <- st.p[1]
  y.st <- st.p[2]
  y.steps <- c(1,2, 3,4,5,6)
  sam.1 <- sample(y.steps, leng, replace=TRUE)
  y1 <- y.st+c(0, sam.1, 0) +1
  x <- x.st + 1:(leng+2)
  sam.2 <- sample(y.steps, leng, replace=TRUE)
  y2 <- y.st-c(0, sam.2, 0) -1
  # Insert begin to sync with get_class_labels
  shift_y <- max(sam.2)
  y1 <- y1 + shift_y
  y2 <- y2 + shift_y
  # Insert end to sync with get_class_labels
  object.pts <- cbind.data.frame(x,y1,y2)
  return(object.pts)
}

create_object_class_B1 <- function(st.p, leng){

  x.st <- st.p[1]
  y.st <- st.p[2]
  y.steps <- c(1,2, 3,4,5,6,7,8)
  sam.1 <- sample(y.steps, leng, replace=TRUE)
  y1 <- y.st+c(0, sam.1, 0) +1
  x <- x.st + 1:(leng+2)
  sam.2 <- sample(y.steps, leng, replace=TRUE)
  y2 <- y.st-c(0, sam.2, 0) -1
  object.pts <- cbind.data.frame(x,y1,y2)
  return(object.pts)
}



create_object_class_B2 <- function(st.p, leng, leng2, gap){

  x.st <- st.p[1]
  y.st <- st.p[2]
  y.steps <- c(1,2, 3,4,5,6,7,8)
  sam.1 <- sample(y.steps, leng, replace=TRUE)

  # curve 1
  y1 <- y.st+c(0, sam.1, 0) +1
  x1 <- x.st + 1:(leng+2)
  object.y1 <- cbind.data.frame(x1,y1)

  # curve 2
  x2 <- x.st +  1:(leng2+2)
  sam.2 <- sample(y.steps, leng2, replace=TRUE)
  y2 <- y.st-c(0, sam.2, 0) -1
  object.y2 <- cbind.data.frame(x2,y2)

  # curve 3
  rat <- runif(1, min=0.3,max=0.7)
  x3.start <- floor(x2[1]+rat*(x2[leng2+2]-x2[1]))
  leng3 <- x2[length(x2)] - x3.start
  x3 <- x3.start:(x3.start+leng3+1)
  y3.start <- y2[leng2]-gap
  y3 <-  y3.start+c(0, sample(y.steps, (leng3), replace=TRUE), 0) +1
  object.y3 <- cbind.data.frame(x3,y3)

  #curve 4
  leng4 <- x.st +leng-x3.start+2
  y4 <- y3.start - c(0, sample(y.steps, leng4, replace=TRUE)) +1
  x4 <- x3.start:(x3.start+leng4)
  object.y4 <- cbind.data.frame(x4,y4)

   return(c(object.y1,object.y2,object.y3,object.y4))
}

give_values_class <- function(pic,object.pts, mu, std){
  x.len <- dim(object.pts)[1]
  x <- object.pts[,1]
  y1 <- object.pts[,2]
  y2 <- object.pts[,3]
  for(i in 1:x.len){
    pic[x[i],y2[i]:y1[i]] <- rnorm((y1[i]-y2[i]+1),mean = mu, sd =std)
  }
  return(pic)
}


give_values_class_B2 <- function(pic, object.pts, mu, std){
  len1 <- length(object.pts$x1)
  len2 <- length(object.pts$x2)
  len3 <- length(object.pts$x3)
  len4 <- length(object.pts$x4)

  x <- object.pts$x1
  y1 <- object.pts$y1
  y2 <- object.pts$y2
  for(i in 1:len2){
    pic[x[i],y2[i]:y1[i]] <- rnorm((y1[i]-y2[i]+1),mean = mu, sd =std)
  }
  x <- object.pts$x3
  y1 <- object.pts$y3
  y2 <- object.pts$y4
  for(i in 1:len3){
    pic[x[i],y2[i]:y1[i]] <- rnorm((y1[i]-y2[i]+1),mean = mu, sd =std)
  }

  x <- object.pts$x1
  y1 <- object.pts$y1
  y2 <- object.pts$y4
  for(i in len2:len1){
    pic[x[i],y1[i]:y2[i-len2+1]]  <- rnorm((y1[i]-y2[i-len2+1]+1),mean = mu, sd =std)
  }
  return(pic)
}

set_parameters <- function(){
  # which subclass for the longer class B object
  b_long <- sample(2:3,1)
  # reverse up down - if rev_sw =1
  # rev_sw <- sample(0:1,1)
  # Changing rev_sw = 1 requires details to be changed
  # Hope to do in future
  rev_sw = 0
  the_rest <- sample(c("A", "B"),3, replace=TRUE)
  classes <- c("B", the_rest)
  class.map <- c("A"=0, "B"=1)
  subclasses <- c(b_long, as.vector(class.map[as.character(the_rest)]))

  width.A <- 20
  width.Bs <- c(width.A,30,50)
  widths <- rep(0,4)
  widths[1] <- ifelse(b_long==2,30,50)
  widths[2:4] <- 20

  lengths <- rep(0,4)
  lengths[1] <- sample(100:150,1)
  lengths[2:4] <- sample(20:30,3)

  xs <- rep(0,4)
  xs[1] <- sample(15:85,1)
  xs[2] <- sample(15:95,1)
  xs[3] <- sample(140:220,1)
  xs[4] <- sample(265:310,1)


  ys <- rep(0,4)
  ys[1] <- sample(135:210,1)
  ys[2] <- sample(20:50,1)
  ys[3] <- sample(20:50,1)
  ys[4] <- sample(30:220,1)

  details.all <- NULL
  details.all$class <- classes
  details.all$subClass <- subclasses
  details.all$length <- lengths
  details.all$width <- widths
  details.all$x <- xs
  details.all$y <- ys
  details.all$rev <- rev_sw

  return(details.all)

}


create_picture <- function(details, pl){
  x <- matrix(rnorm(300*200),ncol=200)
  x2 <- matrix(rnorm(350*250),ncol=250)
  num.objs <- length(details$class)
  for(i in 1:num.objs){
    oo <- create_class(details$class[i],details$subClass[i],details$length[i], details$width[i], details$x[i], details$y[i])

    x2 <- give_values(x2,details$class[i],details$subClass[i],oo)
  }

  if(details$rev==1){
    x2 <- t(x2)
    x2 <- apply(x2,2,rev)
    x2 <- t(x2)
  }
  if(pl){
    image(1:nrow(x2), 1:ncol(x2),x2, xlab="Time", ylab="Location")
  }
  return(x2)
}


create_class <-  function(class,subClass,leng,width,x,y){
  if(class=="A"){
    oo <- create_object_class_A(c(x,y), leng)
  }else{
    if(subClass==1){
      oo <- create_object_class_A(c(x,y), leng)
    }else if(subClass==2){
      oo <-create_object_class_B1(c(x,y), leng)
    }else{
      leng2 <- sample(15:25,1)
      gap <- sample(15:20,1)
      oo <-create_object_class_B2(c(x,y), leng, leng2, gap)
    }
  }
  oo <- fix_boudary(oo)
  return(oo)
}

fix_boudary <- function(oo){
  for(i in 1:length(oo)){
    if(i%%2==1){
      # this is x coordinate - limits are 1 to 350
      uplim <- 349
    }else{
      # this is y coordinate - limits are 1 to 250
      uplim <- 249
    }
    if( sum( ( oo[[i]] < 1 ) | ( oo[[i]] > uplim ) ) > 0 ){
      inds <- which( oo[[i]] < 1 )
      oo[[i]][inds] <- 1

      inds <- which( oo[[i]] > uplim )
      oo[[i]][inds] <- uplim
    }
  }
  return(oo)
}


give_values <- function(x,class, subClass ,oo){
  muAB <- c(4,3)
  sdAB <- c(2,3)
  if(class=="A"){
    x<- give_varying_values_class(x,oo, muAB[1], sdAB[1], "A")
  }else{
    if(subClass!=3){
      x <- give_varying_values_class(x,oo, muAB[2], sdAB[2], "B")
    }else{
      x <-give_varying_values_class_B2(x,oo, muAB[2], sdAB[2])
    }
  }
  return(x)
}


give_varying_values_class <- function(pic,object.pts, mu, std, class){
  x.len <- dim(object.pts)[1]
  if(class=="A"){
    mu.vals <- seq(from=mu, to=(mu+4),length=x.len)
  }else{
    mu.vals <- seq(from=mu, to=(mu+2),length=x.len)

  }
  x <- object.pts[,1]
  y1 <- object.pts[,2]
  y2 <- object.pts[,3]
  for(i in 1:x.len){
    pic[x[i],y2[i]:y1[i]] <- rnorm((y1[i]-y2[i]+1),mean = mu.vals[i], sd =std)
  }
  return(pic)
}


give_varying_values_class_B2 <- function(pic, object.pts, mu, std){
  len1 <- length(object.pts$x1)
  len2 <- length(object.pts$x2)
  len3 <- length(object.pts$x3)
  len4 <- length(object.pts$x4)

  x <- object.pts$x1
  y1 <- object.pts$y1
  y2 <- object.pts$y2
  mu.vals <- seq(from=mu, to=mu+2,length=len2)
  for(i in 1:len2){
    pic[x[i],y2[i]:y1[i]] <- rnorm((y1[i]-y2[i]+1),mean = mu.vals[i], sd =std)
  }

  x <- object.pts$x3
  y1 <- object.pts$y3
  y2 <- object.pts$y4
  mu.vals <- seq(from=mu, to=mu+2,length=len3)
  for(i in 1:len3){
    pic[x[i],y2[i]:y1[i]] <- rnorm((y1[i]-y2[i]+1),mean = mu.vals[i], sd =std)
  }

  x <- object.pts$x1
  y1 <- object.pts$y1
  y2 <- object.pts$y4
  mu.vals <- seq(from=mu, to=mu+3,length=len1)
  for(i in len2:len1){
    pic[x[i],y1[i]:y2[i-len2+1]]  <- rnorm((y1[i]-y2[i-len2+1]+1),mean = mu.vals[i], sd =std)
  }
  return(pic)
}
