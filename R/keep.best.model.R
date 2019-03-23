#'@export
#'@title Keep the best model of a distanceList object
#'
#'
#'@description This function keep the model with the lowest AICc value in a distanceList object.

#'@param x A \code{distanceList} object. See \code{\link{mcds.wrap}}

#'@details
#'Select the model with the smallest AICc value. If more than one model have a Delta AICc equal to zero the uniform models
#'are discaded and the best model is chosen randomly between the remaining models.If the two best models are the uniform model, 
#'one is chose randomly and a warning is printed.  

#'@return
#'An object of class \code{"distanceFit"}

#'@section Author:Christian Roy


keep.best.model <- function(x){
  
  if(class(x)!="distanceList")
    stop("the function can only be applied on distanceList object")  
  
  n.model <- length(x)
  aicc_values <- sapply(1:n.model, function(i){x[[i]]$AIC[3]})
  delta_aic <- aicc_values - aicc_values[order(aicc_values)[1]]
  best.model <- which(delta_aic==0)
  if(length(best.model)==1){
    out <- x[[best.model]]  
    mes <- "Best model selected via AICc ranking\n"
  }else{
    ans <- sapply(1:length(best.model), function(i){charmatch("Uniform", x[[best.model[i]]]$model_fitting$Global$Type, nomatch = 2)-1})  
    if(sum(ans)==0){
      out <- x[[sample(best.model,1)]]  
      mes <- "WARNING: One uniform model has been selected randomly. You should check the observation histogram to make sure that the observation follow the distance sampling assumptions"
    }else{
      best.model <- best.model[which(ans==1)]
      if(length(best.model)==1){
        out <- x[[best.model]] 
        mes <- "A Uniform model with equivalent AICc value was discarded\n"
      }else{
        out <- x[[sample(best.model,1)]]  
        mes <- "Model selected randomly among the model with the lowest AICc values\n"
      }
    }}
  cat(mes)
  return(out)
} 