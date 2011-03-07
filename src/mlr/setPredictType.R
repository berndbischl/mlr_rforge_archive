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
#' @title Set predict type of learner object.
#' @rdname set.predict.pars 

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
    if (learner@desc@type != "classif") {
      stop("Trying to predict ", type, ", but only classifiers support that!")
    }
    if ("prob" == type && !learner@desc@predict["prob"]) {
      stop("Trying to predict probs, but ", learner@desc@id, " does not support that!")
    }
    if ("decision" == type && !learner@desc@predict["decision"]) {
      stop("Trying to predict decision values, but ", learner@desc@id, " does not support that!")
    }
    learner@predict.type = type
    return(learner)
  } 
)


