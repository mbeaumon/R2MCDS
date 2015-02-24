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
  cat("\n")
  cat("\nFiles created by the MCDS engine are saved in the diectory:\n")
  cat(x$path)
}





