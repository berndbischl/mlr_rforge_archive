#' @export
#' @rdname FeatSelControl
makeFeatSelControlExhaustive = function(same.resampling.instance=TRUE, 
  max.features=.Machine$integer.max) {
  
  makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
  max.features=max.features, cl="FeatSelControlExhaustive")
}



