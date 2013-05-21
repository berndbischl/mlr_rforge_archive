#' @export
#' @rdname FeatSelControl
makeFeatSelControlSequential = function(same.resampling.instance=TRUE, method, alpha=0.01, beta=0.01,
  maxit=as.integer(NA), max.features=as.integer(NA)) {
  
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1L, lower=1L, na.ok=TRUE)
  max.features = convertInteger(max.features)
  checkArg(max.features, "integer", len=1L, lower=1L, na.ok=TRUE)  
  checkArg(method, "character", len=1L, choices=c("sfs", "sbs", "sffs", "sfbs"), na.ok=FALSE) 
  checkArg(alpha, "numeric", len=1L, lower=0, na.ok=FALSE)  
  checkArg(beta, "numeric", len=1L, lower=0, na.ok=FALSE)  

  ctrl = mlrTune:::makeOptControl(same.resampling.instance=same.resampling.instance)
  ctrl$maxit = maxit
  ctrl$max.features = max.features
  ctrl$method = method 
  ctrl$alpha = alpha 
  ctrl$beta = beta
  class(ctrl) = c("FeatSelControlSequential", "FeatSelControl", class(ctrl))
  return(ctrl)
}
