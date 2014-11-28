#' @export
#'@title Calculate the effort for each grid cell.
#'
#'
#'@description This function will calculate the effort (i.e the number of time it was visited) for each cells of a grid given a dataset with spatial references.
#'@param Observations data.frame with the observations. 
#'@param Grid.shp a \code{\link{SpatialPolygonsDataFrame}} grid that is overlapping with the observations in the data.frame \cr \emph{une grille de format SpatialPolygonsDataFrame qui chevauche les observations dans le data.frame}
#'@param Latitude which column of the data.frame hold the latitude for the observation 
#'@param Longitude which column of the data.frame hold the longitude for the observation 
#'@param TransectID which colum of the data.frame hold the ID for each independant transect. 
#'@param CellID which colum identify each cell individually in the a \code{\link{SpatialPolygonsDataFrame}} grid
#'@details
#' This function will return the grid appended with new values for each cells.
#'@return
#'  \item{x}{
#'longitude value of the centroid.
#'}
#'  \item{y}{
#'latitude value of the centroid.
#'}
#'  \item{EFFORT}{
#'number of time the cell has been visited.
#'}
#'@section Author:Christian Roy
#'@seealso
#'\code{\link{species.grid}}
#'\code{\link{create.maps}}
#'@examples
#'data(quebec)
#'x<-distance.filter(quebec, transect.id = "WatchID", distance.field = "Distance", distance.labels = c("A", "B", "C", "D"), 
#'                   distance.midpoints = c(25, 75, 150, 250), effort.field = "WatchLenKm", lat.field = "LatStart", 
#'                   long.field = "LonStart", sp.field = "Alpha", date.field = "Date")
#'str(x)
#'END


effort.grid <-
function(Observations=alcidae, Grid.shp=new.grid,
                      Latitude="LatStart",Longitude="LongStart", TransectID="WatchID",
                      CellID="CELL.NUM"){      
      #Keep only the data we want
      stock.df<-subset(Observations, select=c(TransectID, Latitude, Longitude), !duplicated(TransectID))
      #make sure we have the right coordinates
      coordinates(stock.df)<-c(Longitude, Latitude)
      proj4string(stock.df)<-proj4string(Grid.shp)
      #Caculate the number of time the cell has been visited
      visits<-table(over(stock.df,Grid.shp)[,"CELL.NUM"])
      #Extract grid centroids and save them
      Grid.shp@data$x<-sapply(1:length(Grid.shp), function(i){ slot(slot(Grid.shp, "polygons") [[i]], "labpt")[1]})
      Grid.shp@data$y<-sapply(1:length(Grid.shp), function(i){ slot(slot(Grid.shp, "polygons") [[i]], "labpt")[2]})
      #set effort to zero
      Grid.shp@data$EFFORT<-0
      #Assign effort to cell
      Grid.shp@data$EFFORT[match(dimnames(visits)[[1]],Grid.shp@data$CELL.NUM)]<-as.numeric(visits)
      return(Grid.shp)
      #END of the function
      }
