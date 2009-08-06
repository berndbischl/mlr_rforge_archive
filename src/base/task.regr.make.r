#' @include task.regr.r

setGeneric(
		name = "make.regr.task",
		def = function(learner, formula, data, weights) {
			if (missing(weights))
				weights <- rep(1, nrow(data))
			standardGeneric("make.regr.task")
		}
)

#' \code{make.regr.task} defines a regression task for a learner and a data set and is the starting point 
#' for further steps like training, testing, prediciting.
#' 
#' \code{make.regr.task} already performs quite a few tasks: It tries to load required package for the 
#' learner, sets up the learner to deal with a regression problem, gathers information about the features 
#' of the data set and the method, and compares whether they are compatible 
#' (e.g. some methods might not handle NAs or facors). And last but not least it converts integers to numerics 
#' and warns about this. 
#' 
#' @param learner [character] \cr 	Specifies the learner. The naming convention is to add the sufixes ".regr" 
#' 									(or ".classif") to the learner name, if it can handle both types. 
#' @param formula [formula] \cr		a symbolic description of the model to be fitted, see \link{formula}.
#' @param data [data.frame] \cr 	an optional data frame containing the variables in the model.
#' @param weights [numeric] \cr 	an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector.
#' 
#' @return An object of class \code{\linkS4class{regr.task}}.
#' 
#' @export
#' @rdname make.regr.task
#' 
#' @usage make.regr.task(learner, formula, data, weights)
#'
#' @examples
#' library(mlbench)
#' data(BostonHousing)
#' # define a regression task for a Gradient Boosting Machine for regression for the data set BostonHousing
#' rt <- make.regr.task("gbm.regr", data = BostonHousing, formula = medv~.)
#' 
#' @seealso \linkS4class{wrapped.learner}, \linkS4class{regr.task}, \link{train}, \link{predict}
#' 
#' @title make.regr.task


setMethod(
		f = "make.regr.task",
		signature = signature(
				learner = "character", 
				formula = "formula", 
				data = "data.frame", 
				weights = "numeric" 
		),
		
		def = function(learner, formula, data, weights) {
			wl <- new(learner)
			rt <- new("regr.task", wrapped.learner=wl, formula=formula, data=data, weights=weights)
			return(rt)
		}
)
