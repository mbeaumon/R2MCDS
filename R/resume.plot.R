#' @export
#'@title Create a PDF or png file that summarize the ouput of the Distance software. 
#'
#'
#'@description The file contains a chi-square test table, a predicted density table, a summary of the the detection function, a summary for the model and a histogram of detection. 
#'@param model The output of a distance model fitted with the unmarked package (of class "\code{unmarkedFitDS}") or a model fitted with the \code{\link{distance.wrap}} function (of class "\code{distanceFit}" or "list").
#'@param sampling.units Character object describing what are the sampling units. "Watches" by default.
#'@param obs.units Character object describing what are the observation units. "Birds" by default.
#'@param stratum Logical value. Should the results be presented by stratum when available (\code{TRUE} ?
#'@param subset Single character object corresponding to a model name when models are splitted.
#'@param title A title for the plot.
#'@param subtitle A subtitle for the plot.
#'@param file Name for the file.
#'@param directory Where to save the output.
#'@param PDF Save the output either as a PDF (TRUE) or a png (FALSE).
#'@details
#'This function creates a visual summary of the output file saved as a PDF or png file. See the help files for \code{\link{distance.wrap}} for other examples.
#'@return
#'This function will prints all the summary tables and an histogram with the predicted detection function in on a 11 X 8 inches PDF. 
#'@references 
#'S.T. Buckland, D.R. Anderson, K.P. Burnham, J.L Laake, D.L. Borchers and L. Thomas. 2001. \emph{Introduction to Distance Sampling}. Estimating abundance of biological populations. Oxford University Press.
#'@section Author:Christian Roy, Francois Rousseu
#'@examples
#'data(alcidae)
#'alcidae.output<-unmarked.wrap(alcidae,Trn.Value="Count", Dist.class="Distance", Keep.class=c("A", "B", "C", "D"),
#'                             Breaks=c(0,50,100,200,300),  Trn.ID="WatchID",  Trn.length="WatchLenKm", Resume=T)
#'par(ask=T)
#'resume.plot(alcidae.output[[1]],title="Distribution modelling of ECSAS data",subtitle="2007-2009",file="tempo", directory="C:/temp/distance", PDF=T)
#'resume.plot(alcidae.output[[2]],title="Distribution modelling of ECSAS data",subtitle="2007-2009",file="tempo", directory="C:/temp/distance", PDF=T)
#'resume.plot(alcidae.output[[3]],title="Distribution modelling of ECSAS data",subtitle="2007-2009",file="tempo", directory="C:/temp/distance", PDF=T)
#'par(ask=F)
#'
#'#Not run
#'#To print then result to a PDF file
#'# pdf(file="alcidae_halfnormal.pdf", width=11, height=8)
#'# resume.plot(alcidae.output[[1]])
#'# dev.off()
#'###END

resume.plot <-
function (x, ...) {
	UseMethod("resume.plot", x)
}
