print.summary.distanceFit <- function(x, ...){
  cat("Summary of a distance sampling object fitted via the MCDS engine:\n")
  print(x$Parameters,row.names = FALSE)
  cat(paste("\n\nDetection function used : ",x$Type, sep=""))
  cat("\nParameters of the detection function:\n")
  print(x$Detection[,-1],row.names = FALSE)
  cat("\n\nDensity estimates for the entire survey area:\n")
  print(x$Density[,-1], row.names = FALSE)
}









