cluster_sizeMCDS <- function(x){

  trim <-  function(x){gsub("^\\s+|\\s+$","",x)}
  ans <- list()
  
  adj <- regexpr("No size bias adjustment",x)
  
  
  if(max(adj)==-1){
    w <- grep(" Regression Estimates",x)
    if(any(w)){
      vnames1 <- trim(sapply((w+2):(w+5),function(i){substr(x[i],2,12)}))
      vnum1 <- as.numeric(sapply((w+2):(w+5),function(i){substr(x[i],16,26)}))
      vnames2 <- trim(sapply((w+2):(w+5),function(i){substr(x[i],31,44)}))
      vnum2 <- as.numeric(sapply((w+2):(w+5),function(i){substr(x[i],46,60)}))
      ans[[1]] <- data.frame(Parameters=vnames1[1:2], Estimate=vnum1[1:2],sd=vnum2[1:2])
      ans[[2]] <- c(vnum1[3:4],vnum2[3:4])
      names(ans[[2]]) <- c(vnames1[3:4],vnames2[3:4])
    
      g <- grep("Expected cluster size =",x) 
      ans[[3]] <- c(as.numeric(substr(x[g],25,30)), as.numeric(substr(x[g],58,65)))
      names(ans[[3]]) <- c(trim(substr(x[g],2,22)),trim(substr(x[g],35,55)))
    
      g.2 <- grep("Average cluster size",x)
      ans[[4]] <- as.numeric(dtable(x,w=g.2,char=c(g.2+1,g.2+1), nbcol=5))
      names(ans[[4]])<- c("Average cluster size","%CV","df","Lower 95%CI", "Upper 95%CI")    
    }else{
      ans<-"Expected cluster size = 1 with no variance"
    }
  }else{
   ans <- "No bias adjustement was necessary for this model" 
  }
    return(ans)
}
