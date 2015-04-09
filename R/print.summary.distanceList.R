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
