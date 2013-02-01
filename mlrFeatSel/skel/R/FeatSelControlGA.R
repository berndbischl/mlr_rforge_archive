#' @export
#' @rdname FeatSelControl
makeFeatSelControlGA = function(same.resampling.instance=TRUE,
  maxit=100L, max.features=as.integer(NA), rate=0.5, init=10, size=5) {
  
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
    maxit=maxit, max.features=max.features, 
		cl="FeatSelControlGA")
	ctrl$rate = rate 
	ctrl$init = init 
	ctrl$size = size
  return(ctrl)
}
