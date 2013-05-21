#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance=TRUE,
  maxit=100L, max.features=as.integer(NA), prob=0.5) {
  
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=FALSE)
  max.features = convertInteger(max.features)
  checkArg(max.features, "integer", len=1L, lower=1L, na.ok=TRUE)  
  checkArg(prob, "numeric", len=1L, lower=0, upper=1, na.ok=TRUE)  
  
  ctrl = mlrTune:::makeOptControl(same.resampling.instance=same.resampling.instance)
  ctrl$maxit = maxit
  ctrl$max.features = max.features
  ctrl$prob = prob
  class(ctrl) = c("FeatSelControlRandom", "FeatSelControl", class(ctrl))
  return(ctrl)
}
