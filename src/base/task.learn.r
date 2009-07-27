#' @include data.desc.r
#' @include wrapped.learner.r
roxygen()


#'  \describe{	
#' A learning task is the general description object for a machine learning experiment, which contains 
#' all i al setup for a classification task. It mainly includes the type of 
#' the classifier (e.g. lda), a dataframe and a formula. As this is just an abstract base class, 
#' you should not instantiate it directly but rather inherit from it in the learn.task classes of
#' your specific classifiers. }
#' 
#' \cr\cr\bold{Slots:}
#'  \describe{	
#'   \item{\code{name [character]}}{Name of the classifier}
#'   \item{\code{package [character]}}{R package where classifier is defined}
#'   \item{\code{train.fct [function] }}{Function used in above package to train a regular model in the package (see \code{\link{train}})}
#'   \item{\code{train.fct.pars [list]}}{Named list of parameters which are fixed in the above train.fct. NB: These are _NOT_ 
#' 		hyperparamters of the classifier but rather parameters which are fixed for the whole 
#' 		experiment and should be fixed at the beginning for convenience (Example would be the 
#'  	tolerance parameter in lda.)}
#'   \item{\code{predict.fct [function]}}{Function used in above package to predict new data with a trained model (see \code{\link{predict}}) }
#'   \item{\code{predict.fct.pars [list]}}{Named list of parameters which are fixed in the above predict.fct. See train.fct.pars 
#' 		(example would be the method parameter in predict.lda.)}
#'   \item{\code{predict.fct.trafo [function]}}{A function which, when applied to an output of the predict function, returns the vector of 
#' 		predicted class memberships the default function returns the output x if x is a factor, else
#' 		returns x$class if possible or gives a warning)}
#'   \item{\code{desc [\linkS4class{classif.props}]}}{Contains logical values describing which kind of data 
#' 	   	the classifier can deal with e.g. whether it supports characters.
#' 		This is necessary to make sure that the classifier can deal with the given data(see \code{\linkS4class{classif.props}})}
#'   \item{\code{data [dataframe]}}{ Dataframe which includes all the data for the task }
#'   \item{\code{formula [formula]}}{Specifies what output and inputs, r
#'     e.g. Species ~ Petal.Length + Sepal.Length for iris}
#'   \item{\code{data.desc [\linkS4class{data.desc}]}}{Contains logical values describing properties of the dataframe e.g. wheter it has characters 
#' 	   or missing values (see desc and \code{\linkS4class{data.desc}})}
#'   \item{\code{dummy.classes [logical]}}{Does the predict function need a class column in the dataframe 
#' 	   for prediction? If TRUE but no class column is avaible in the data a null column is generated 
#' 	   in predict (default is FALSE).}
#'  }
#' 
#' @examples
#' # How to inherit from this class:
#' 
#' library(MASS)
#' 
#' setClass("lda")
#' setIs("lda", "external.model")
#' 
#' setClass("t.lda", contains="learn.task")
#' setClass("m.lda", contains="model")
#' 
#' @title learn.task
#' @export



setClass(
		"learn.task",
		representation(
				wrapped.learner = "wrapped.learner",
				data = "data.frame",
				weights = "numeric",
				formula = "formula",
				data.desc = "data.desc", 
				resampled = "numeric"
		)
)


#---------------- constructor---- -----------------------------------------------------


setMethod(
		f = "initialize",
		signature = "learn.task",
		def = function(.Object, check.function, wrapped.learner, data, weights, formula) {
			
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(wrapped.learner))
				return(.Object)					
			
			.Object@wrapped.learner <- wrapped.learner
			.Object@data <- data
			.Object@weights <- weights
			.Object@formula <- formula
			cn <- .Object["target.name"]
			.Object@data.desc <- new("data.desc", data, cn)
			.Object@resampled <- numeric(length=0)
			
			
			check.result <- check.function(.Object)
			if (check.result$msg != "") {
				stop(check.result$msg)
			}
			else {
				.Object@data <- check.result$data
				.Object@data.desc <- new("data.desc", .Object@data, cn)
			}
			return(.Object)
		}
)


setMethod(
		f = "[",
		signature = "learn.task",
		def = function(x,i,j,...,drop) {
			if (i == "target.name"){
				return(as.character(x@formula)[2])
			}
			if (i == "target.col"){
				return(which(colnames(x@data) == x["target.name"]))
			}
			
			if (i == "targets") {
				if (missing(j))
					j = 1:nrow(x@data)
				return(x@data[j, x["target.name"]])
			}
			if (i == "input.names"){
				return(attr(terms(x@formula, data=x@data), "term.labels"))
			}
			
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)




setMethod(
		f = "print",
		signature = "learn.task",
		def = function(x, ...) {
			cat(as.character(x))
		}
)

setMethod(
		f = "show",
		signature = "learn.task",
		def = function(object) {
			cat(as.character(object))
		}
)


#---------------- restrict.learn.task -----------------------------------------------------

restrict.learn.task <- function(learn.task, subset) {
	learn.task@data <- learn.task@data[subset,]
	learn.task@resampled <- subset
	return(learn.task)
}



