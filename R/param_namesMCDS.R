param_namesMCDS <- function(x){
  
  trim <-  function(x){gsub("^\\s+|\\s+$","",x)}
  ans <- list()
  
  w1 <- grep(" Model",x)
  w2 <- grep("  Parameter",x)
  
  ans[[1]] <- dtable(x,char=c(w1[length(w1)]+3,w2[length(w2)]-1), nbcol=12)
  ans[[1]] <- ans[[1]][which(ans[[1]][,1]=="Parameter"),]
  return(ans)
}