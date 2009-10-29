#' @include wrapped.learner.classif.r
roxygen()


#' Wrapped learner for Adaboost.M1 from package \code{adabag} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{minsplit}}{Minimum number of observations that must exist in a node in order for a split to be attempted.}			
#' 		\item{\code{cp}}{Complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.}
#' 		\item{\code{maxdepth}}{Maximum depth of any node of the final tree, with the root node counted as depth 0. Defaults to the number of classes.} 
#' }
#' @title adaboost
#' @seealso \code{\link[adabag]{adaboost.M1}}
#' @export
setClass(
		"adaboost", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

#' Constructor.
#' @title Adaboost Constructor
setMethod(
		f = "initialize",
		signature = signature("adaboost"),
		def = function(.Object, data, target, type="class") {
			train.fct <- "adaboost.M1"
			predict.fct <- "predict.boosting"
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = FALSE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="boost", learner.pack="adabag", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				wrapped.learner="adaboost", 
				target="character", 
				data="data.frame", 
				weights="numeric", 
				costs="matrix", 
				type = "character" 
		),
		
		def = function(wrapped.learner, target, data, weights, costs, type,  ...) {
			f = as.formula(paste(target, "~."))
			adaboost.M1(f, data=data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				wrapped.learner = "adaboost", 
				task = "classif.task", 
				wrapped.model = "wrapped.model", 
				newdata = "data.frame", 
				type = "character" 
		),
		
		def = function(wrapped.learner, task, wrapped.model, newdata, type, ...) {
			# stupid adaboost
			newdata[, task["target"]] <- factor(rep(1, nrow(newdata)), levels=task["class.levels"])
			p = predict(wrapped.model["learner.model"], newdata=newdata, ...)
			return(as.factor(p$class))
		}
)	




