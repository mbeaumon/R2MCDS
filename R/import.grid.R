#' @export
#'@title Create a grid from existing centroids values.
#'
#'
#'@description The function will create a \code{\link{SpatialPolygonsDataFrame}} from a set of previously saved centroïds of a grid.
#'@param file file	the name of the file with the centroid values. Each row of the table appears as one line of the file. If it does not contain an \emph{absolute} path, the file name is \emph{relative} to the current working directory, \code{\link{getwd}}().
#'@param header header  a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: \env{header} is set to \env{TRUE} if and only if the first row contains one fewer field than the number of columns.
#'@param sep sep  the field separator character. Values on each line of the file are separated by this character.
#'@param x  name of the field containing the latitude values in text file.
#'@param y  name of the field containing the longitude values in text file.
#'@param CRS  coordinate reference system ID for the imported centroids values.
#'@section Author:Christian Roy
#'@seealso
#'\code{\link{read.table}}\cr
#'\code{\link{CRS-class}}\cr
#'@examples
#'data(quebec)
#'x<-distance.filter(quebec, transect.id = "WatchID", distance.field = "Distance", distance.labels = c("A", "B", "C", "D"), 
#'                   distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", lat.field = "LatStart", 
#'                   long.field = "LonStart", sp.field = "Alpha", date.field = "Date")
#'str(x)
#'END

import.grid <-
function(file, header=TRUE, sep="", x=x, y=y, CRS){
                   #basic import format
                   import<-read.csv(file, header=header, sep=sep)
                   #transorm the file into a spatial points dataframe
                   coordinates(import) <- ~ x+y
                   #transform into a grid
                   output<-as.SpatialPolygons.GridTopology(points2grid(import))
                   #use the correct projection
                   proj4string(output)<-CRS(CRS)
                   return(output)
                   }
