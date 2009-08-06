#' @include task.classif.r
roxygen()

setGeneric(
		name = "make.classif.task",
		def = function(learner, formula, data, weights, type) {
			if (missing(weights))
				weights <- rep(1, nrow(data))
			if (missing(type))
				type <- "class"
			standardGeneric("make.classif.task")
		}
)


#' \code{make.classif.task} defines a classification task for a learner and a data set and is the starting point 
#' for further steps like training, testing, prediciting.
#' 
#' \code{make.classif.task} already performs quite a few tasks: It tries to load required package for the 
#' learner, sets up the learner to deal with a classification problem, gathers information about the features 
#' of the data set and the method, and compares whether they are compatible 
#' (e.g. some methods might not handle NAs or facors). And last but not least it converts integers to numerics 
#' and warns about this. 
#' 
#' @param learner [character] \cr 	Specifies the learner. The naming convention is to add the sufixes ".classif" 
#' 									(or ".regr") to the learner name, if it can handle both types. 
#' @param formula [formula] \cr		a symbolic description of the model to be fitted, see \link{formula}.
#' @param data [data.frame] \cr 	an optional data frame containing the variables in the model.
#' @param weights [numeric] \cr 	an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector.
#' @param type [character] \cr 		specifies the type of the predicitons - either probability ("probs") 
#' 									or class ("class").
#' 

#' 
#' @return An object of class \code{\linkS4class{classif.task}}.
#' 
#' @export
#' @rdname make.classif.task
#' 
#' @usage make.classif.task(learner, formula, data, weights, type)
#'
#' @examples
#' data(iris) 
#' # define a classification task for a decision tree (rpart) for the data set iris
#' ct <- make.classif.task("rpart.classif", data = iris, formula = Species ~.)
#' 
#' @seealso \linkS4class{wrapped.learner}, \linkS4class{classif.task}, \link{train}, \link{predict}
#'  
#' @title make.classif.task


setMethod(
		f = "make.classif.task",
		signature = signature(
				learner = "character", 
				formula = "formula", 
				data = "data.frame", 
				weights = "numeric", 
				type = "character"
		),
		
		def = function(learner, formula, data, weights, type) {
			wl <- new(learner)
			ct <- new("classif.task", wrapped.learner=wl, formula=formula, data=data, weights=weights, type=type)
			return(ct)
		}
)
