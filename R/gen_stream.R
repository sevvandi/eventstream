#' Generates a two dimensional data stream containing events of two classes.
#'
#' This function generates a two-dimensional data stream containing events of two classes. The data stream can be saved as separate files with images by specifying the argument \code{folder}.
#'
#' @param n The number of files to generate. Each file consists of a 350x250 data matrix.
#' @param folder If this is set to a local folder, the data matrices are saved in \code{folder/data}, the images are saved in \code{folder/pics} and the event details are saved in \code{folder/summary}.  The event details are needed to obtain the class labels of events, when event extraction is done.
#' @param sd This specifies the seed.
#' @param vis If \code{TRUE}, the images are plotted.
#'
#' @details
#' There are events of two classes in the data matrices : A and B.  Events of class A have only one shape while events of class B have three different shapes, including class A's shape. This was motivated from a real world example. The details of events of each class are given below.
#' \tabular{lrr}{
#' Feature \tab class A   \tab class B \cr
#' Starting cell/pixel values \tab \code{N(4,3)} \tab \code{N(2,3)} \cr
#' Ending cell/pixel values  \tab \code{N(8,3)}  \tab \code{N(4,3)} \cr
#' Maximum age of event - shape 1 \tab \code{U(20,30)} \tab \code{U(20,30)} \cr
#' Maximum age of event - shape 2  \tab \code{NA}  \tab \code{U(100,150)} \cr
#' Maximum age of event - shape 3  \tab \code{NA} \tab \code{U(100,150)} \cr
#' Maximum width of event - shape 1 \tab \code{U(20,26)} \tab \code{U(20,26)} \cr
#' Maximum width of event - shape 2  \tab \code{NA}  \tab \code{U(30,38)} \cr
#' Maximum width of event - shape 3  \tab \code{NA} \tab \code{U(50,58)} \cr
#' }
#'
#'@return
#'A list with following components:
#'   \item{\code{data}}{The data stream returned as a data frame.}
#'   \item{\code{details}}{A data frame containing the details of the events: their positions, class labels, etc.. .  This is needed for identifying class labels of events during event extraction.}
#'
#'
#' @examples
#' out <- gen_stream(1, sd=15)
#' zz <- as.matrix(out$data)
#' image(1:nrow(zz), 1:ncol(zz),zz, xlab="Time", ylab="Location")
#'
#'@seealso \code{\link{stream_from_files}}.
#'@export


gen_stream <- function(n,  folder=NULL, sd=1, vis=FALSE){
  set.seed(sd)

  if(!missing(folder)){
    last_char <- substr(folder, nchar(folder), nchar(folder))
    if(last_char=="/"){
      folder <- substr(folder, 1, nchar(folder)-1)
    }
    if(!dir.exists(folder)){
      stop("Invalid directory!")
    }

    files_list <- list.files(folder)
    if(length(files_list)>0){
      data_true <- sum(files_list %in% "data")
      pic_true <- sum(files_list %in% "pic")
      sum_true <- sum(files_list %in% "summary")
      if( data_true + pic_true+ sum_true > 0){
        stop("Directory contains similar sub-directories. Please give a suitable directory.")

      }
    }

    data_folder <- folder
    dir.create(paste(folder, "/data", sep=""))
    dir.create(paste(folder, "/summary", sep="") )
    dir.create(paste(folder, "/pic", sep="") )


    save_pic <- TRUE

    data_folder <- paste(folder, "/data/", sep="")
    summary_folder <- paste(folder, "/summary/", sep="")
    pics_folder <- paste(folder, "/pic/", sep="")

  }else{
    save_pic <- FALSE

  }

  ll <- 1 # this index is for the data frame which has everything
  All.details <- data.frame(filename=as.character(), class=as.character(), subclass=numeric(), length=numeric(), width=numeric(),  file_x=numeric(), file_y=numeric(),stream_x=double(), stream_y=double() ,stringsAsFactors = FALSE)
  out <- list()

  ll <- 1 # this index is for the data frame which has all.details
  for(kk in 1:n){
    details <- set_parameters()
    x <- create_picture(details, vis)

    pp <- 100 +kk
    filename <- paste("Gen_",pp,".jpg", sep="")
    if(save_pic){
      jpeg(paste(pics_folder, filename, sep="" ))
      image(1:nrow(x), 1:ncol(x),x, xlab="Time", ylab="Location")
      dev.off()

    }


    details$file_x <- details$x
    details$file_y <- details$y

    details$stream_x <- (kk-1)*350 + details$x
    details$stream_y <- details$y

    pp <- 100 +kk
    if(!missing(folder)){
      filename <- paste("Gen_",pp,".csv", sep="")
      write.csv(x,paste(data_folder,filename, sep=""), row.names = FALSE)
    }

    if(kk==1){
      xx <- x
    }else{
      xx <- rbind.data.frame(xx, x)
    }

    for(i in 1:length(details$class)){
      All.details[ll,1:2] <- c(filename,details$class[i])
      All.details[ll,3:9] <- cbind.data.frame(details$subClass[i], details$length[i], details$width[i], details$file_x[i],details$file_y[i],details$stream_x[i],details$stream_y[i] )
      ll <- ll +1
    }
  }

  if(!missing(folder)){
    write.csv(All.details, paste(summary_folder,"Details_Of_Events_In_Take_2_Folder_3.csv", sep=""),row.names=FALSE)
  }

  out$data <- xx
  out$details <- All.details
  return(out)
}


