#' @export
#'@title Plot the density estimates for a given dataset and grid.
#'
#'
#'@description This function plots output of the \code{\link{species.grid}} function. The user has the option to plot only the cells that have been visited or all the cells. 
#'@param Observations.grid output of a \code{\link{species.grid}}
#'@param Observations which column of the Observation.grid should be used to plot abundance 
#'@param only.visits \env{TRUE} if the user want only the cells of the grid that have been visited. \env{FALSE} will plot all the cells of the grid.
#'@param Land \env{TRUE} if the user want to plot an additional shapefile over the grid. \env{FALSE} if the user want only the grid to be plotted. 
#'@param Land.shapefile name of the object the user want to plot over the grid. Must be an object of the class \code{\link{SpatialPolygonsDataFrame}} 
#'@param Title Title for the figure. Must be a \code{\link{character}} vector.
#'@details This function return a plot with the numbers of observations per cell and a scale to help visualisation. The upper limit for the scale is 19.
#'@section Author:Christian Roy
#'@examples
###Import a dataset
#'data(alcidae)
#'###Create  a grid 
#'new.grid<-create.grid(Latitude=c(45, 54), Longitude=c(-70, -56), Grid.size=c(25000, 25000), Clip=FALSE, projection=NULL)
#'###calculate the density of alcidea for the grid
#'alcidae.grid<-species.grid(alcidae, Grid.shp=new.grid, Selection="Alpha",
#'                      Code=c("ALCI"), Density="Count",Latitude="LatStart",Longitude="LongStart",
#'                      Cell.id="CELL.NUM", Factors=F)
#'#plot a map with only the cells that have been visited
#'create.maps(Observations.grid=alcidae.grid, Observations="Count", only.visits=TRUE, Land=FALSE)
#'#plot a map with all the cells
#'create.maps(Observations.grid=alcidae.grid, Observations="Count", only.visits=FALSE, Land=FALSE)
#'#use the create.maps function to visualize effort in each grid cell. 
#'create.maps(Observations.grid=alcidae.grid, Observations="EFFORT", only.visits=FALSE, Land=FALSE, Title="Number of time a cell has been visited")
#'#END

create.maps <-
function(Observations.grid, Observations, only.visits=TRUE, Land=FALSE,
                      Land.shapefile, Title=NULL){

                    if(Land==T){
                      if(class(Land.shapefile)[1]!="SpatialPolygonsDataFrame")
                      stop("Land.shapefile must be a shapefile")
                      if(proj4string(Observations.grid)==proj4string(Land.shapefile))
                      stop("Land.shapefile and Observation.grid must be in the same projection")
                    }else{
                      if(class(Land)!="logical")
                      print("Clip was set to FALSE therefore the grid was not plotted")
                    }
                    if(class(Observations.grid@data[,Observations])!= "numeric"){
                       Observations.grid@data[,Observations] <- as.numeric(Observations.grid@data[,Observations])
                       }

                    max.seen<-max(Observations.grid@data[,Observations])
                    max.seen<-ifelse(max.seen ==0, 1, max.seen)
                    if(max.seen <= 20){
                        up.scale<-ceiling(max.seen)*3 + 1
                        my.colors<-c("white", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
                        my.colors2<-c("#0000ff01", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
                      }else{
                        up.scale<- 20*3
                        my.colors<-c("white", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
                        my.colors2<-c("#0000ff01", rev(heat_hcl(up.scale, c = c(80, 30), l = c(30, 90), power = c(1/5, 2)))[seq(1,up.scale, by=3)])
                    }
                      par(pin=c(8.5, 11), mar=c(1, 4, 4, 2) +0.1)
                      layout(mat=matrix(1 :2, nrow=2, ncol=1), heights=c(6, 1))
                      par(pin=c(8.5, 11), mar=c(1, 4, 4, 2) +0.1)
                      if(only.visits==T){
                        dataset<-Observations.grid[Observations.grid$VISITED>0,]
                      }else{
                        dataset<-Observations.grid
                      }
                      plot(dataset, col=my.colors[dataset@data$VISITED+1], axes=T)
                      if(is.null(Title)==F){
                        title(Title)
                      }
                      plot.col <- ifelse(ceiling(dataset@data[,Observations])>=20, 20, ceiling(dataset@data[,Observations]))
                      plot(dataset, col=my.colors2[plot.col+1], add=T)
                      if(Land==T){
                        plot(Land.shapefile, col="cornflowerblue", add=T)
                      }
                      par(mar=c(2,1,1,1))
                      color.scale(my.colors)
                      index<-seq(1,length(my.colors),1)
                      place<-(c(0,(index/length(my.colors))[1:(length(my.colors) - 1)]) + index/length(my.colors))/2
                      mtext(text="Not\nVisited", side=1, at=place[1], padj=0.5, cex=0.75)
                      my.text<-c(seq(0, (length(my.colors2)-3)), paste(length(my.colors2)-2,"+", sep=""))
                      mtext(text=my.text, side=1, at=place[2:length(place)], cex=0.75)
                #END of the function
                }

