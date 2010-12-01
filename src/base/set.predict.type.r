#' Set predict type of learner object - whether classes, probabilities or decision values should be predicted. 
#' 
#' @param learner [\code{\linkS4class{learner}}]\cr 
#'        Learner object.   
#' @param type [string] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to
#'        predict. Default is "response". "decision" is experimental. Ignored for
#'        regression.	 
#' 		    
#' @return \code{\linkS4class{learner}} with changed prediction behaviour.
#' @exportMethod set.predict.type
#' @title Set predict type of learner object.
#' @rdname set.predict.pars 

setGeneric(
  name = "set.predict.type",
  def = function(learner, type) {
    standardGeneric("set.predict.type")
  }
)

#' @rdname set.predict.type 
setMethod(
  f = "set.predict.type",
  
  signature = signature(
    learner="learner", 
    type="character" 
  ),
  
  def = function(learner, type) {
    if ("prob" == type && !learner["probs"]) {
      stop("Trying to predict probs, but ", learner["id"], " does not support that!")
    }
    if ("decision" == type && !learner["decision"]) {
      stop("Trying to predict decision values, but ", learner["id"], " does not support that!")
    }
    learner@predict.type = type
    return(learner)
  } 
)


