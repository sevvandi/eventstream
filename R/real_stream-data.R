#'A data stream from a real world application
#'
#'A dataset containing fibre optic cable signals.  A pulse is periodically sent through the cable and this results in a data matrix where each horizontal row (\code{real_stream[x, ]}) gives the strength of the signal at a fixed location \code{x}, and each vertical column (\code{real_stream[ ,t]}) gives the strength of the signal along the cable at a fixed time \code{t}.
#'
#'@format A matrix with 587 rows and 379 columns.
"real_stream"
