#' Set predict type of classification learner object - whether classes, probabilities or decision values should be predicted. 
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'        Learner object.   
#' @param type [character(1)] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to
#'        predict. Default is "response". "decision" is experimental. Ignored for
#'        regression.	 
#' 		    
#' @return \code{\linkS4class{Learner}} with changed Prediction behaviour.
#' @exportMethod setPredictType
#' @rdname setPredictType 
#' @title Set predict type of learner object.

setGeneric(
  name = "setPredictType",
  def = function(learner, type) {
    standardGeneric("setPredictType")
  }
)

#' @rdname setPredictType 
setMethod(
  f = "setPredictType",
  
  signature = signature(
    learner="Learner", 
    type="character" 
  ),
  
  def = function(learner, type) {
    if (type != "response" && learner@properties[["type"]] != "classif") {
      stop("Trying to predict ", type, ", but only classifiers support that!")
    }
    if ("prob" == type && !learner@properties[["prob"]]) {
      stop("Trying to predict probs, but ", learner@id, " does not support that!")
    }
    if ("decision" == type && !learner@properties[["decision"]]) {
      stop("Trying to predict decision values, but ", learner@id, " does not support that!")
    }
    learner@predict.type = type
    return(learner)
  } 
)


