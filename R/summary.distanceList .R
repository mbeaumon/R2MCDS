summary.distanceList <- function(x, ...){
  ans <- lapply(x, summary.distanceFit)
  class(ans) <- "summary.distanceList"
  return(ans)
}