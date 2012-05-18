#' Predict new data with an R learner. 
#'
#' Mainly for internal use. Predict new data with a fitted model.
#' You have to implement this method if you want to add another learner to this package.
#' 
#' You implementation must adhere to the following:
#' The model must be fitted on the subset of \code{.task} given by \code{.subset}. All parameters
#' must in \code{...} must be passed to the underlying training function. 
#' Mainly for internal use. Predicts new data with WrappedModel. 
#' You have to implement this method if you want to add another learner to this package. 
#' @title Internal prediction method for learner. 
#' @param .learner [\code{\link{RLearner}}]\cr  
#'   Wrapped learner. 
#' @param .model [\code{\link{WrappedModel}}]\cr
#'   Model produced by training. 
#' @param .newdata [\code{data.frame}]\cr
#'   New data to predict. Does not include target column.
#' @param ... [any]\cr
#'   Additional parameters, which need to be passed to the underlying predict function.
#' @return For classification: Either a factor for type \dQuote{response} or a matrix for
#'   type \dQuote{prob}. In the later case the columns must be named with the class labels.
#'   For regressions: 
#' @export
#' FIXME return type for regression
#' FIXME rereead and details
predictLearner = function(.learner, .model, .newdata, ...) {
  UseMethod("predictLearner")
}

