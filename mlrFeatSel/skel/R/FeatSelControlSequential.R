#' @export
#' @rdname FeatSelControl
makeFeatSelControlSequential = function(same.resampling.instance=TRUE, method, alpha=0.01, beta=0.01,
  maxit=as.integer(NA), max.features=as.integer(NA)) {
  
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
                            maxit=maxit, max.features=max.features, alpha=alpha, beta=beta, cl="FeatSelControlSequential")
  ctrl$method = method
  ctrl$alpha = alpha
  ctrl$beta = beta
  return(ctrl)
}
