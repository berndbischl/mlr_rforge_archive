# Mean response infill criterion.
# 
# @param points [\code{data.frame}]\cr 
#   Infill criterion function.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model used for prediction.
# @return Mean response infill criterion.
infillCritMeanResponse = function(points, model) {
  predict(model, newdata=points)$data$response
}

infillCritNaiveEI = function(points, model) {
  p = predict(model, newdata=points)$data
  p$response - 3 * p$se
}