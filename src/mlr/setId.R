#' Set id of a learner object. 
#'
#' Please remember that learner ids must be unique. 
#' 
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'        Learner object.
#' @param id [\code{character(1)}] \cr
#'       New id.
#' 		    
#' @return \code{\linkS4class{Learner}} with changed id.
#'
#' @example
#' cl <- makeLearner("classif.logreg")
#' cl <- setId(cl, "My Logistic Regression Learner")
#' print(cl)
#'
#' @exportMethod setId
#' @title Set id of learner object.
#' @rdname setId 

setGeneric(name = "setId",
           def = function(learner, id) {
             standardGeneric("setId")			
           })


#' @rdname setId 
setMethod(f = "setId",	
          signature = signature(
            learner="Learner", 
            id="character"
            ),          
          def = function(learner, id) {
            stopifnot(is.character(id) && length(id) == 1)
            learner@id = id
            return(learner)
          })
