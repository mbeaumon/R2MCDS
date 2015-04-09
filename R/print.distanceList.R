print.distanceList <- function(x, ...){
  
  cat("List of Distance sampling object fitted via the MCDS engine:\n")
  cat("\n")
  for(i in 1:length(x)){
    cat(paste("Results for ", names(x[i]),":", sep=""))
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
  cat("\nFiles created by the MCDS engine are saved in the diectory:\n")
  cat(x[[1]]$path)
}