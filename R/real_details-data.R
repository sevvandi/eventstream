#'A dataset containing the details of class A events in the dataset real_stream.
#'
#'This dataset contains the location of class A events in the real_stream dataset. This can be used for classifying the events in real_stream.
#'
#'@format A data frame with 4 rows and 3 variables:
#'\describe{
#'  \item{filename}{Orignal file name}
#'  \item{class}{class of event, A or B}
#'  \item{file_x}{\code{y} coordinate of file, relating to the location of event}
#'  \item{file_y}{\code{x} coordinate of file, relating to the start time of event }
#'  \item{stream_x}{\code{x} coordinate of \code{real_stream}, relating to the start time of event }
#'  \item{stream_y}{\code{y} coordinate of \code{real_stream}, relating to the location of event}
#'}
#'
"real_details"
