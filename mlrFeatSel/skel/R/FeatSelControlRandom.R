#' @export
#' @rdname FeatSelControl
makeFeatSelControlRandom = function(same.resampling.instance=TRUE, 
  max.features=.Machine$integer.max, prob=0.5) {
  
  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
                     max.features=max.features, cl="FeatSelControlRandom")
  ctrl$prob = prob
  return(ctrl)
}


