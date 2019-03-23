#'Summary and print methods
#'
#'Summary and print methods for \code{distanceFit} and \code{distanceList} objects
#'
#'See the help file of \code{\link{mcds.wrap}}
#'
#'@rdname summary
#'@export
print.distanceFit <- function(x, ...){
  
  cat("Distance sampling object fitted via the MCDS engine:\n")
  cat("\n")
  cat(paste("Detection function: ", unlist(strsplit(x$model_fitting$Global$Type, ","))[1],sep=""))
  cat("\n")
  cat("\nEstimated abundance in covered region: ")
  cat(paste(x$density_estimate$Global[x$density_estimate$Global[,"Parameters"]=="N",c("Estimates")],
            " (",
            x$density_estimate$Global[x$density_estimate$Global[,"Parameters"]=="N",c("95% Lower")],
            "-",
            x$density_estimate$Global[x$density_estimate$Global[,"Parameters"]=="N",c("95% Upper")],
            ") 95% CI",
            sep=""))
  cat("\n\n")
  cat(paste("Model AICc = ",as.numeric(x$AIC[3]),sep=""))
  cat("\n")
  cat("\nFiles created by the MCDS engine are saved in the directory:\n")
  cat(x$path)
}

#'@rdname summary
#'@export
print.distanceList <- function(x, ...){
  
  cat("List of Distance sampling object fitted via the MCDS engine:\n")
  cat("\n")
  for(i in 1:length(x)){
    cat(paste("Results for model", names(x[i]),":", sep=""))
    cat(paste("\nDetection function: ", unlist(strsplit(x[[i]]$model_fitting$Global$Type, ","))[1],sep=""))
    cat("\nEstimated abundance in covered region: ")
    cat(paste(x[[i]]$density_estimate$Global[x[[i]]$density_estimate$Global[,"Parameters"]=="N",c("Estimates")],
              " (",
              x[[i]]$density_estimate$Global[x[[i]]$density_estimate$Global[,"Parameters"]=="N",c("95% Lower")],
              "-",
              x[[i]]$density_estimate$Global[x[[i]]$density_estimate$Global[,"Parameters"]=="N",c("95% Upper")],
              ") 95% CI",
              sep=""))
    cat("\n")
    cat(paste("Model AICc = ",x[[i]]$AIC[3],sep=""))
    cat("\n\n")
  }
  cat("\nFiles created by the MCDS engine are saved in the directory:\n")
  cat(x[[1]]$path)
}

#'@rdname summary
#'@export
print.summary.distanceFit <- function(x, stratum=F, ...){
  cat("Summary of a distance sampling object fitted via the MCDS engine:\n")
  print(x$Parameters,row.names = FALSE, right = FALSE)
  cat(paste("\n\nDetection function used : ",x$Type, sep=""))
  cat("\nParameters of the detection function:\n")
  print(x$Detection[,-1],row.names = FALSE)
  cat("\n")
  if(class(x$Key)=="list"){
    for(j in 1:length(x$Key[[1]])){ cat(paste(paste(x$Key[[1]][j,!is.na(x$Key[[1]][j,])], collapse=" "),"\n", sep=""))} 
  }
  cat("Density estimates for the Global survey area:\n")
  print(x$Density[,-1], row.names = FALSE)
  cat("\n")
  cat(paste("For stratified models use 'stratum=T' to get indivudal strata estimates"))
  cat("\n")
  cat(paste("Model AICc = ",x$AICc,sep=""))
}

#'@rdname summary
#'@export
print.summary.distanceList <- function(x, ...){
  cat("Summary of a list of distance sampling object fitted via the MCDS engine:\n")
  print(x[[1]]$Parameters[-5,],row.names = FALSE)
  for(i in 1:length(x)){
    cat(paste("\nResults for ",names(x)[i], sep=""))
    cat(paste("\n",names(x)[i]," Clusters observed = ",x[[i]]$Parameters[5,2], sep=""))
    cat(paste("\n\nDetection function used for ",names(x)[i]," :",x[[i]]$Type, sep=""))
    cat("\nParameters of the detection function for",names(x)[i],":\n")
    print(x[[i]]$Detection[,-1],row.names = FALSE)
    cat("\n\n",names(x)[i],"Density estimates for the entire survey area:\n")
    print(x[[i]]$Density[,-1], row.names = FALSE)
    cat("\n")
    cat(paste("Model AICc = ",x[[i]]$AICc,sep=""))
    cat("\n\n")
  }
}

#'@rdname summary
#'@export
summary.distanceFit <- function(x, ...){
  ans <- list(Detection=x$parameter_estimates$Global,
              Density=x$density_estimate$Global,
              Parameters= x$model_fitting$Global$Parameters,
              Key = x$covar_key,
              Type=x$model_fitting$Global$Type,
              AICc=x$AIC[3])
  class(ans) <- "summary.distanceFit"
  return(ans)
}

#'@rdname summary
#'@export
summary.distanceList <- function(x, ...){
  ans <- lapply(x, summary.distanceFit)
  class(ans) <- "summary.distanceList"
  return(ans)
}




