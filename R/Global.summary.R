#' @export
#'@title Create a PDF file that summarize the output of the Distance software. 
#'
#'
#'@description The file contains a chi-square test table, a predicted density table, a summary of the the detection function, a summary for the model and a histogram of detection. 
#'@param model The output of a distance model fitted with the \code{\link{distance.wrap}} function (of class "\code{distanceFit}" or "list").
#'@param species vector containing the name of species in each output.
#'@param file Name for the file.
#'@param directory Where to save the output.
#'@details
#'This function creates a visual summary of the output file saved as a PDF. See the help files for \code{\link{distance.wrap}} for other examples.
#'@return
#'This function will prints all the summary tables and an histogram with the predicted detection function in on a 11 X 8 inches PDF. If Global summary is applied on a list in will produce a serie of PDF. 
#'@references 
#'S.T. Buckland, D.R. Anderson, K.P. Burnham, J.L Laake, D.L. Borchers and L. Thomas. 2001. \emph{Introduction to Distance Sampling}. Estimating abundance of biological populations. Oxford University Press.
#'@section Author:Christian Roy
#'@examples
#'### Import and filter data
#'data(alcidae)
#'alcids<-filterECSAS(alcidae)
#'
#'### Set arguments and parameters
#'path<-"c:/temp/distance"
#'pathMCDS<-"C:/Program Files (x86)/Distance 6"
#'breaks<-c(0,50,100,200,300)
#'STR_LABEL<-"STR_LABEL"
#'STR_AREA<-"STR_AREA"
#'SMP_LABEL<-"WatchID"
#'SMP_EFFORT<-"WatchLenKm"
#'DISTANCE<-"Distance"
#'SIZE<-"Count"
#'
#'### Run analysis with the MCDS engine. Here, the WatchID is used as the sample.
#'x<-distance.wrap(alcids,path=path,pathMCDS=pathMCDS,breaks=breaks,STR_LABEL=STR_LABEL,STR_AREA=STR_AREA,SMP_LABEL=SMP_LABEL,SMP_EFFORT=SMP_EFFORT,DISTANCE=DISTANCE,SIZE=SIZE,verbose=FALSE)
#'global.summary(model=x, species="alcids", file="alcidae_global", directory="C:/temp/distance") 
#'END

global.summary <-
function (model, ...) {
  UseMethod("global.summary", model)
}