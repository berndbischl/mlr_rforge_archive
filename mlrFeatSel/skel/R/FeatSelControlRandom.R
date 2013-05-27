#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance=TRUE,
  maxit=100L, max.features=as.integer(NA), prob=0.5) {
  
  #FIXME do more arg checks, eg maxit should not be na?
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
    maxit=maxit, max.features=max.features, cl="FeatSelControlRandom")
  ctrl$prob = prob
  return(ctrl)
}
