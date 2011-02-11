#' Set id of learner object. 
#' 
#' @param learner [\code{\linkS4class{learner}}]\cr 
#'        Learner object.   
#' @param id [string] \cr
#'       New id.
#' 		    
#' @return \code{\linkS4class{learner}} with changed id.
#' @exportMethod setId
#' @title Set id of learner object.
#' @rdname setId 

setGeneric(
		name = "setId",
		def = function(learner, id) {
			standardGeneric("setId")			
		}
)

#' @rdname setId 
setMethod(
		f = "setId",
		
		signature = signature(
				learner="learner", 
				id="character" 
		),
		
		def = function(learner, id) {
			learner@id = id
			return(learner)
		} 
)



