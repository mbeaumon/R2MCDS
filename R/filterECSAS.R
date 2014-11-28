#' @export
#'@title Filter data collected according to the Eastern Canada Seabirds At Sea (ECSAS) standardized protocol for pelagic seabird surveys.
#'
#'
#'@description Prepares the raw data gathered by the ECSAS for a distance analysis.
#'@param x A \code{\link{data.frame}} containing observations.
#'@param dist2m Logical value. Should the distance classes be transformed in meters?
#'@details
#'The following column must be in \code{x}: "WatchLenKm","InTransect","Distance" and "Date".
#'When "WatchLenKm" = 0, observations are eliminated. Only observations within the transect ("InTransect" = 1) are kept. Transects for
#'which "Alpha" = "" (no species names) will also be kept because they are transects that were done but where no observations were recorded.
#'Only distance class "A", "B", "C" and "D" will be kept and if \code{dist2m = TRUE},
#'they will be transformed to 25, 75, 150 and 250, respectively. Missing distance will also be kept if the observation corresponds to an empty transect.
#'Observations for which there is no distance or coordinates will be eliminated. The "Date" column will be transformed in the yyyy-mm-dd format.
#'@section Author:Francois Rousseu
#'@seealso \code{\link{alcidae}}, \code{\link{quebec}}
#'@examples
#'data(quebec)
#'x<-filterECSAS(quebec)
#'str(x)
#'#End

filterECSAS <-
function(x,dist2m=TRUE){	
	x<-x[x[,"WatchLenKm"]>0,] 
	x<-x[!is.na(x[,"LatStart"]),]
	y<-x
	y[,"Distance"]<-NA
	y[,"Alpha"]<-""
	x<-x[!(x[,"Distance"]=="" & x[,"Alpha"]!=""),] #eliminates observations recorded without a distance
	x[,"Distance"]<-ifelse(x[,"Distance"]=="",NA,as.character(x[,"Distance"])) #writes NA when there is no distance, when nothing in the transect
	x<-x[x[,"Distance"]%in%c("A","B","C","D",NA),]
	x[,"Distance"]<-as.character(x[,"Distance"])
	if(dist2m){
		x[,"Distance"]<-ifelse(x[,"Distance"]=="A",25,x[,"Distance"])
		x[,"Distance"]<-ifelse(x[,"Distance"]=="B",75,x[,"Distance"])
		x[,"Distance"]<-ifelse(x[,"Distance"]=="C",150,x[,"Distance"])
		x[,"Distance"]<-ifelse(x[,"Distance"]=="D",250,x[,"Distance"])
		x[,"Distance"]<-as.numeric(x[,"Distance"])
	}
	x<-x[x[,"InTransect"]==1 | x[,"Alpha"]=="",] #keep observations that are in the transect or empty transects/WatchID
	y<-y[!y[,"WatchID"]%in%x[,"WatchID"],] #keep only WatchID that are not already in x
	x<-rbind(x,unique(y)) #add empty transects with outside distances to the main data.frame
	date<-sapply(strsplit(sapply(strsplit(as.character(x[,"Date"])," "),function(i){i[1]}),"/"),function(j){
		res<-rev(j)
		paste(c(res[1],formatC(as.numeric(res[2:3]),width=2,flag="0")),collapse="-")
	})
	x[,"Date"]<-date
	x
}
