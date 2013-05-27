#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance=TRUE,
                                maxit=as.integer(NA), max.features=as.integer(NA), crossover.rate=0.5, mutation.rate=0.2, mu=10, lambda=5) {
  
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=FALSE)
  checkArg(crossover.rate, "numeric", len=1L, lower = 0, upper = 1, na.ok=FALSE)
  checkArg(mutation.rate, "numeric", len=1L, lower = 0, upper = 1, na.ok=FALSE)
  mu = convertInteger(mu)
  checkArg(mu, "integer", len=1L, lower=1L, na.ok=FALSE)  
  lambda = convertInteger(lambda)
  checkArg(lambda, "integer", len=1L, lower=1L, upper=mu, na.ok=FALSE)  
  
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
    maxit=maxit, max.features=max.features, 
    crossover.rate=crossover.rate, mutation.rate=mutation.rate, mu=mu, lambda=lambda,
		cl="FeatSelControlGA")
  return(ctrl)
}
