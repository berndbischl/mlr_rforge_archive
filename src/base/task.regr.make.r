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
#' for further steps like training, predicting new data, resampling and tuning.
#' 
#' \code{make.regr.task} already performs quite a few tasks: It tries to load required package for the 
#' learner, sets up the learner to deal with a regression problem, gathers information about the features 
#' of the data set and the method, and compares whether they are compatible 
#' (e.g. some methods might not handle NAs or factors). It also might perform some data conversions 
#' in the data.frame, like coverting integer features to numerics, but will generally  
#' warn about this.
#' 
#' List of supported learning algorithms. The naming conventions are to add the package name as a prefix if
#' a learner is implemented in different packages and the suffix ".regr" if it can handle more than a
#' regression task.  
#' 
#' \itemize{ 
#' 		\item{\code{\linkS4class{kernlab.svm.regr}}}{SVMs from kernlab package}  
#' }
#' 
#' @param learner [\code{\link{character}}] \cr
#'  	  Specifies the learner. See the list below in the details section.
#' @param formula [\code{\link{formula}}] \cr
#'        A symbolic description of the model to be fitted.
#' @param data [\code{\link{data.frame}}] \cr
#'   	  A data frame containing the variables in the model.
#' @param weights [\code{\link{numeric}}] \cr
#'        An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
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
#' @seealso \code{\linkS4class{wrapped.learner}}, \code{\linkS4class{regr.task}}, \code{\link{train}}, \code{\link{predict}}
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
