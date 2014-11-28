#' @export
#'@title Fit the generalized distance sampling model described by Chandler et al. (2011) with the 3 basic detection keys for a given dataset.
#'
#'
#'@description This function allow the user to perform a Distance analysis using the unmarked package without having to reformat his data. The function also run the three different analyses by default and return all results in one list. 
#'@param Dataset Name of the input dataset. 
#'@param Trn.Value which column in the data.frame holds the information for the densities. 
#'@param Dist.class which column in the data.frame holds the information for the distance class. 
#'@param Keep.class which class of observations should be used for the analysis.
#'@param Breaks what are the intervals used for the distance analysis. 
#'@param Trn.ID which column in the data.frame holds the unique ID for each transect.
#'@param Trn.length which column in the data.frame holds the length of each transect.
#'@param model run only the selected model. Options are \env{"halfnorm"}, \env{"exp"}, \env{"hazard"}.
#'@param Resume if \env{Resume} is set to \env{TRUE} the analysis will be performed as one giant transect instead of several small transect.
#'@return
#'The function return a list containing the three models supported by the \code{\link{distsamp}} function in the package \pkg{unmarked}.
#'Each model composing the list is of class \code{"unmarkedFitDS"}.
#'@section Author:Christian Roy
#'@references
#'S.T. Buckland, D.R. Anderson, K.P. Burnham, J.L Laake, D.L. Borchers and L. Thomas. 2001. \emph{Introduction to Distance Sampling}. Estimating abundance of biological populations. Oxford University Press.\cr
#'Royle, J. A., D. K. Dawson, and S. Bates. 2004. \emph{Modeling abundance effects in distance sampling}. Ecology 85:1591–1597\cr
#'Fiske, I. and R. B. Chandler. 2011. \emph{unmarked: An R package for fitting hierarchical models of wildlife occurrence and abundance}. Journal of Statistical Software 43:1–23.\cr 
#'@seealso
#'\code{\link{distsamp}}
#'\code{\link{distance.wrap}}
#'\code{\link{resume.plot}}
#'@examples
###Import the data
#'data(alcidae)
#'
#'###Check the naïve detection histogram
#'hist.wrap(alcidae,Trn.Value="Count", Dist.class="Distance", Keep.class=c("A", "B", "C", "D"),
#'          Breaks=c(0,50,100,200,300), Trn.ID="WatchID",  Trn.length="WatchLenKm", Resume=F)
#'
#'###Run the three basic unmarked models
#'alcidae.output<-unmarked.wrap(alcidae,Trn.Value="Count", Dist.class="Distance", Keep.class=c("A", "B", "C", "D"),
#'                             Breaks=c(0,50,100,200,300),  Trn.ID="WatchID",  Trn.length="WatchLenKm", Resume=T)
#'
#'###Look at the structure of the output
#'str(alcidae.output)
#'
#'####Look at the summary of each output
#'summary(alcidae.output[[1]])
#'summary(alcidae.output[[2]])
#'summary(alcidae.output[[3]])
#'#plot the resume plot for the best model
#'resume.plot(alcidae.output[[2]])
#'#END

unmarked.wrap <-
function(Dataset, Trn.Value, Dist.class, Keep.class, Breaks, Trn.ID, 
                        Trn.length, model=NULL, Resume=F){
               #Fail check
               if(class(Dataset[, Trn.ID])!="numeric"){
                  Dataset[, Trn.ID]<-as.numeric(Dataset[, Trn.ID])
               }
               
               distance<-subset(Dataset, !duplicated(Dataset[,Trn.ID]))[,Trn.length]
               index<-which(dimnames(tapply(Dataset[,Trn.Value],  INDEX=list(watch=Dataset[,Trn.ID], distance=Dataset[,Dist.class]), FUN=sum))[[2]]%in%Keep.class)
               #create the observation matrix
               mat.trn<-tapply(Dataset[,Trn.Value],  INDEX=list(watch=Dataset[,Trn.ID], distance=Dataset[,Dist.class]), FUN=sum)[,index]
               #replace NA with 0
               mat.trn[is.na(mat.trn)]<-0
               #Create one giant transect if TRUE
               if(Resume==T){
                mat.trn<-t(as.matrix(colSums(mat.trn)))
                distance <- sum(distance)
               }
               #Create the datframe
               my.unmarked<-unmarkedFrameDS(y = mat.trn, siteCovs =NULL, dist.breaks = Breaks, tlength=distance*1000, survey = "line", unitsIn = "m")
               models<-c("halfnorm","exp","hazard")
               #Run the function for each model and save as a list
               fm.list<-lapply(1:3, function(i){
                            cmd<-paste("distsamp(formula = ~1 ~ 1, data = my.unmarked, keyfun =", "'",models[i],"'", ", output = 'density', unitsOut = 'kmsq', method = 'Nelder-Mead')",sep="")
                            fm<-try(eval(parse(text=cmd)), silent=T)
                            return(fm)
                            })
               names(fm.list)<-c("hn(.)lam(.)", "exp(.)lam(.)", "haz(.)lam(.)")
               return(fm.list)
               #End of the function
               }
