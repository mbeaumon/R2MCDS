#' @export
#'@title Calculate the density of animal in each grid cell.
#'
#'
#'@description This function calculate the mean density of observation by cells of a grid given a dataset with spatial references.
#'@param Observations data.frame holding the observations.
#'@param Grid.shp a \code{\link{SpatialPolygonsDataFrame}} grid that  overlapping with the obseravtions in the data.frame.
#'@param Selection which column of the data.frame should be used to select the observations.
#'@param Code which observations in the column \env{Selection} should be used.
#'@param Density which column of the data.frame hold the number of animals observed for a given row.
#'@param Latitude which column of the data.frame hold the latitude for the observation.
#'@param Longitude which column of the data.frame hold the longitude for the observation.
#'@param Cell.id which column identify each cell individually in the \code{\link{SpatialPolygonsDataFrame}} grid.
#'@param Factors if set to \env{TRUE} the mean density will also be reported as a class given by the \env{my.breaks} and \env{my.class} values.  
#'@param my.breaks cut points for the class of density.
#'@param my.labels name of the labels for each class of density.
#'@param dots extra commands
#'@details
#'This function returns the grid appended with new values for each cells.
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
#'  \item{VISITED}{
#'binary value. if the cell has been visited 1 otherwise 0.
#'}
#'  \item{Count}{
#'mean density of animal observed in the cell.
#'}
#'@section Author:Christian Roy
#'@examples
#'###Import a dataset
#'data(alcidae)
#'###Create  a grid 
#'new.grid<-create.grid(Latitude=c(45, 54), Longitude=c(-70, -56), Grid.size=c(25000, 25000), Clip=F, clip.shape=canshp, projection=NULL)
#'###calculate the density of alcidae for the grid
#'alcidae.grid<-species.grid(alcidae, Grid.shp=new.grid, Selection="Alpha",
#'                      Code=c("ALCI"), Density="Count",Latitude="LatStart",Longitude="LongStart",
#'                      Cell.id="CELL.NUM", Factors=F)
#'#END

species.grid <-
function(Observations=alcidae, Grid.shp=new.grid, Selection="Alpha",
                      Code=c("HERG"),Density="Count",Latitude="LatStart",Longitude="LongStart",
                      Cell.id="CELL.NUM", Factors=TRUE, my.breaks=c(-1,0,5,10,15,1000),
                      my.labels=c(1,2,3,4,5),...){

      if (Factors==T){
        if((length(my.labels)+1)!=length(my.breaks))
        stop("You need one more breaks than you have labels")
      }

      #make an effort grid
      Effort <- effort.grid(Observations=Observations, Grid.shp=Grid.shp,
                            Latitude=Latitude, Longitude=Longitude)
      #subset the data
      sp.dataframe<-subset(Observations, Observations[,Selection]%in%Code)
      coordinates(sp.dataframe)<-c(Longitude,Latitude)
      # Give right coordinates
      proj4string(sp.dataframe)<-proj4string(Grid.shp)
      # assgign each observations to a cell in a list
      sp.grid.list<-over(Grid.shp, sp.dataframe, returnList = T)
      sp.grid.df<-do.call("rbind", lapply(1:length(sp.grid.list), function(i){
                   if(nrow(sp.grid.list[[i]])==0){
                   c(i,rep(0, times=length(Density)))
                   }else{
                      if(length(Density)==1){
                      c(i,mean(sp.grid.list[[i]][,Density]))
                      }else{
                      #c(i,colSums(sp.grid.list[[i]][,Density]))
                       c(i,colMeans(sp.grid.list[[i]][,Density]))
                      }
                   }
              }))
      #
      Grid.shp@data$x<- Effort@data$x
      Grid.shp@data$y<- Effort@data$y
      Grid.shp@data$EFFORT<- Effort@data$EFFORT
      #VISITED indique si la cellule a été visité une au moin un fois
      Grid.shp@data$VISITED<-ifelse(Effort@data$EFFORT>0, 1, 0)
      #SP.TOTAL est le nombre d'oiseaux observés au total
      for(i in 1:length(Density)){
          Grid.shp@data[,Density[i]]<-sp.grid.df[, i+1]
      }
      #SP.CLASS est une reclassifcication du nombre d'oiseaux observés et 5 classes
      if (Factors==T){
        names.vec<-paste(Density, "class" , sep=".")
        for(i in 1:length(Density)){
            Grid.shp@data[,names.vec[i]]<-cut(sp.grid.df[,i+1], breaks=my.breaks, labels=my.labels, include.lowest=F)
        }
      }
      return(Grid.shp)
      #END of the function
      }
