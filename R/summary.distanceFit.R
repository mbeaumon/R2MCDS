summary.distanceFit <- function(x, ...){
  ans <- list(Detection=x$parameter_estimates$Global,
              Density=x$density_estimate$Global,
              Parameters= x$model_fitting$Global$Parameters,
              Key = x$covar_key,
              Type=x$model_fitting$Global$Type,
              AICc=x$AIC[3])
  class(ans) <- "summary.distanceFit"
  return(ans)
}