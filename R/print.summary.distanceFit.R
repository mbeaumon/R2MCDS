print.summary.distanceFit <- function(x, ...){
  cat("Summary of a distance sampling object fitted via the MCDS endgine:\n")
  print(x$Parameters,row.names = FALSE)
  cat("\n\nParameters of the detection function:\n")
  print(x$Detection[,-1],row.names = FALSE)
  cat(paste("Detection function used : ",x$Type, sep=""))
  cat("\n\nDensity Estimates:\n")
  print(x$Density[,-1], row.names = FALSE)
}









