summary.distanceFit <- function(x, ...){
  ans <- list(Detection=x$parameter_estimates$Global,
              Density=x$density_estimate$Global,
              Parameters= x$model_fitting$Global$Parameters,
              Type=x$model_fitting$Global$Type)
  class(ans) <- "summary.distanceFit"
  return(ans)
}