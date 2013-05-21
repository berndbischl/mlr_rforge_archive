#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance=TRUE,
  maxit=as.integer(NA), max.features=as.integer(NA), crossoverRate=0.5, mutationRate=0.2, mu=10, lambda=5) {

  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=TRUE)
  max.features = convertInteger(max.features)
  checkArg(max.features, "integer", len=1L, lower=1L, na.ok=TRUE)  
  checkArg(crossoverRate, "numeric", len=1, lower = 0, upper = 1, na.ok=FALSE)
  checkArg(mutationRate, "numeric", len=1, lower = 0, upper = 1, na.ok=FALSE)
  mu = convertInteger(mu)
  checkArg(mu, "integer", len=1L, lower=1L, na.ok=FALSE)  
  lambda = convertInteger(lambda)
  checkArg(lambda, "integer", len=1L, lower=1L, upper=mu, na.ok=FALSE)  
  
  ctrl = mlrTune:::makeOptControl(same.resampling.instance=same.resampling.instance)
  ctrl$maxit = maxit
  ctrl$max.features = max.features
  ctrl$crossoverRate = crossoverRate 
  ctrl$mutationRate = mutationRate 
  ctrl$mu = mu
  ctrl$lambda = lambda
  class(ctrl) = c("FeatSelControlGA", "FeatSelControl", class(ctrl))
  return(ctrl)
}
