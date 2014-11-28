#' @export
#'@title Plot a detection histogram of apparent density.
#'
#'
#'@description This function will allow the user to visualise the histogram of observations in function of distance class and re-bin his observations if necessary before the analysis.
#'@param x  A \code{\link{data.frame}} containing observations.
#'@param Dataset what is the name of the dataset that will be used for the analysis.
#'@param Trn.Value which column in the data.frame hold the information for the abundance.
#'@param Dist.class which column in the data.frame hold the information for the distance class.
#'@param Keep.class which class of observations should be used for the analysis.
#'@param Breaks what are the intervals for the distance analysis.
#'@param Trn.ID which column in the data.frame hold the unique ID for each transect.
#'@param Trn.length which column in the data.frame hold the length of each transect.
#'@param Resume if \env{Resume} is set to \env{TRUE} the analysis will be performed as one giant transect instead of several small transect.
#'@details
#'When "WatchLenKm" = 0, observations are eliminated. Transects for which "Alpha" = "" (no species names) 
#'will be kept because they are transects that were done but where no observations were recorded.
#'Observations for which there is no distance or coordinates will be eliminated. 
#'The "Date" column will be transformed in the yyyy-mm-dd format.
#'@section Author:Christian Roy
#'@seealso \code{\link{unmarked.wrap}}
#'@examples
#'###Import the data
#'data(alcidae)
#'
#'###Check the naïve detection histogram
#'hist.wrap(alcidae,Trn.Value="Count", Dist.class="Distance", Keep.class=c("A", "B", "C", "D"),
#'          Breaks=c(0,50,100,200,300), Trn.ID="WatchID",  Trn.length="WatchLenKm", Resume=F)
#'#END


hist.wrap <-
function(Dataset, Trn.Value, Dist.class, Keep.class, Breaks, Trn.ID, Trn.length, Resume=F){
               #fail check
               if(class(Dataset[, Trn.ID])!="numeric"){
                  Dataset[, Trn.ID]<-as.numeric(Dataset[, Trn.ID])
               }
               #Keep only one observation by transect
               distance<-subset(Dataset, !duplicated(Dataset[,Trn.ID]))[,Trn.length]
               index<-which(dimnames(tapply(Dataset[,Trn.Value],  INDEX=list(watch=Dataset[,Trn.ID], distance=Dataset[,Dist.class]), FUN=sum))[[2]]%in%Keep.class)
               #Create the observation matrix
               mat.trn<-tapply(Dataset[,Trn.Value],  INDEX=list(watch=Dataset[,Trn.ID], distance=Dataset[,Dist.class]), FUN=sum)[,index]
               #replace NA by 0
               mat.trn[is.na(mat.trn)]<-0
               #Combine all the results in one giant transect if TRUE
               if(Resume==T){
                mat.trn<-t(as.matrix(colSums(mat.trn)))
                distance <- sum(distance)
               }
               #Create the unmarked dataframe
               my.unmarked<-unmarkedFrameDS(y = mat.trn, siteCovs =NULL, dist.breaks = Breaks, tlength=distance*1000, survey = "line", unitsIn = "m")
               #Make the histrogram
               hist(my.unmarked)
               #END of the function
               }
