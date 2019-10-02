#'A dataset containing NO2 data for 2014
#'
#'This dataset contains smoothed NO2 data from March to September 2014
#'
#'@format An array of 4 x 179 x 360 dimensions.
#'\describe{
#'  \item{Dimension 1}{Each \code{NO2_2014[t, , ]} contains NO2 data for a given month with \code{t=1} corresponding to March and \code{t=7} corresponding to September}
#'  \item{Dimensions 2,3}{Each \code{NO2_2014[ ,x, y]} contains NO2 concentration for a given position in the world map.}
#'
#'}
#'
#'@source \url{https://neo.sci.gsfc.nasa.gov/view.php?datasetId=AURA_NO2_M}
"NO2_2014"
