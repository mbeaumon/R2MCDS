print.summary.distanceFit <- function(x, ...){
  cat("Summary of a distance sampling object fitted via the MCDS engine:\n")
  print(x$Parameters,row.names = FALSE)
  cat(paste("\n\nDetection function used : ",x$Type, sep=""))
  cat("\nParameters of the detection function:\n")
  print(x$Detection[,-1],row.names = FALSE)
  cat("\n")
  if(class(x$Key)=="list"){
    for(j in 1:length(x$Key[[1]])){ cat(paste(paste(x$Key[[1]][j,!is.na(x$Key[[1]][j,])], collapse=" "),"\n", sep=""))} 
  }
  cat("Density estimates for the entire survey area:\n")
  print(x$Density[,-1], row.names = FALSE)
  cat("\n")
  cat(paste("Model AICc = ",x$AICc,sep=""))
}









