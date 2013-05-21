#' @export
#' @rdname FeatSelControl
makeFeatSelControlExhaustive = function(same.resampling.instance=TRUE, 
  maxit=as.integer(NA), max.features=as.integer(NA)) {

  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=TRUE)
  max.features = convertInteger(max.features)
  checkArg(max.features, "integer", len=1L, lower=1L, na.ok=TRUE)  
  
  ctrl = mlrTune:::makeOptControl(same.resampling.instance=same.resampling.instance)
  ctrl$maxit = maxit
  ctrl$max.features = max.features
  class(ctrl) = c("FeatSelControlExhaustive", "FeatSelControl", class(ctrl))
  return(ctrl)
}