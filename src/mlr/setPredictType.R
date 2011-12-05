#' Set the type of prediction the learner should return.
#' Possible prediction types are labels / numeric response, class probabilities (including labels)
#' and standard errors (including numeric response). 
#' @title Set predict type of learner object.
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner object.   
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} or \dQuote{prob}.
#'   Regression: \dQuote{response} or \dQuote{se}.
#'   Default is \dQuote{response}.
#' @return \code{\linkS4class{Learner}} with changed Prediction behaviour.
#' @seealso \code{\link{setThreshold}} to alter the threshold used for prediction.
#' @example
#'   cl <- makeLearner("classif.logreg")
#'   cl <- setPredictType(cl, "response")
#' @export
setPredictType = function(learner, predict.type) {
  checkArg(learner, "Learner")
  checkArg(predict.type, choices= c("response", "prob", "se"))
  if (predict.type == "prob" && !learner@properties[["prob"]])
    stop("Trying to predict probs, but ", learner@id, " does not support that!")
  if (predict.type == "se" && !learner@properties[["se"]])
    stop("Trying to predict se, but ", learner@id, " does not support that!")
  learner@predict.type = predict.type
  return(learner)
}
