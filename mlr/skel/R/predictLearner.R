#' Mainly for internal use. Predicts new data with WrappedModel. 
#' You have to implement this method if you want to add another learner to this package. 
#' @title Internal prediction method for learner. 
#' @param .learner [\code{\linkS4class{Learner}}] \cr  
#'   Wrapped learner from this package. 
#' @param .model [\code{\link{character}}] \cr
#'   Model produced by training. 
#' @param .newdata [\code{\link{data.frame}}] \cr
#'   New data to predict.
#' @param ... [any] \cr
#'   Additional parameters, which need to be passed to the underlying train function.
#' @return Model of the underlying learner.
#' @export
predictLearner = function(.learner, .model, .newdata, ...) {
  UseMethod("predictLearner")
}

