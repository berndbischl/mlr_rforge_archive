#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance=TRUE,
  maxit=as.integer(NA), max.features=as.integer(NA), crossoverRate=0.5, mutationRate=0.2, mu=10, lambda=5) {
  
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
    maxit=maxit, max.features=max.features, 
		cl="FeatSelControlGA")
  ctrl$crossoverRate = crossoverRate 
  ctrl$mutationRate = mutationRate 
	ctrl$mu = mu
	ctrl$lambda = lambda
  return(ctrl)
}
