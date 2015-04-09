AIC_MCDS <- function(x){
  w1 <- grep("LnL",x)
  w2 <- grep("BIC",x)
  
  ans <- dtable(x,char=c(w1,w2), nbcol=2)
  res <- as.numeric(ans[,2])
  names(res) <- ans[,1]
  return(res)
}