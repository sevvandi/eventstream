#' Generates a two dimensional data stream from data files in a given folder.
#'
#' @param folder The folder with the data files.
#'
#' @examples
#' \dontrun{
#' folder <- tempdir()
#' out <- gen_stream(2, folder = folder)
#' stream <- stream_from_files(paste(folder, "/data", sep=""))
#' dim(stream)
#' unlink(folder, recursive = TRUE)
#' }
#'@seealso \code{\link{gen_stream}}.
#'@export


stream_from_files <- function(folder){
  last_char <- substr(folder, nchar(folder), nchar(folder))
  if(last_char=="/"){
    folder <- substr(folder, 1, nchar(folder)-1)
  }
  if(!dir.exists(folder)){
    stop("Invalid directory!")
  }
  files_list <- list.files(folder, pattern="csv")
  if(length(files_list)==0){
    stop("No data files in directory!")
  }
  for(i in 1:length(files_list) ){
    dat<- read.csv(paste(folder, "/",files_list[i], sep=""))
    if(i==1){
      stream <- dat
    }else{
      stream <- rbind.data.frame(stream, dat)
    }
  }
  return(stream)
}
