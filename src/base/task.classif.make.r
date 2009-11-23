#' @include task.classif.r
roxygen()

setGeneric(
		name = "make.classif.task",
		def = function(target, formula, data, weights, costs, type) {
#			if (is.character(learner))
#				learner <- new(learner)
#			if (!is(learner, "wrapped.learner.classif"))
#				stop("Trying to constuct a classif.task from a non classification learner: ", class(learner))
			
			if (missing(weights))
				weights <- rep(1, nrow(data))
			if (missing(type))
				type <- "class"
			if (missing(costs)) {
				if (missing(target))
					tn <- as.character(formula)[2]
				else
					tn <- target						
				n <- length(levels(data[,tn]))
				costs <- matrix(1,n,n) - diag(1,n)
			}		
			standardGeneric("make.classif.task")
		}
)


#' \code{make.classif.task} defines a classification task for a data set and is the starting point 
#' for further steps like training, predicting new data, resampling and tuning.
#' 
#' \code{make.classif.task} already performs quite a few tasks: It tries to load the required package for the 
#' learner, sets up the learner to deal with a classification problem, gathers information about the features 
#' of the data set and the method, and compares whether they are compatible 
#' (e.g. some methods might not handle NAs or factors). And last but not least it might perform some data conversions 
#' in the data.frame, like coverting integer features to numerics or integer classes to factors, but will generally  
#' warn about this. 
#' 
#' List of supported learning algorithms. The naming conventions are to add the package name as a prefix if
#' a learner is implemented in different packages and the suffix ".classif" if it can handle more than a
#' classification task.  
#' 
#' \itemize{ 
#' 		\item{\code{\linkS4class{adaboost}}}{Boosting from adabag package}
#' 		\item{\code{\linkS4class{kknn.classif}}}{k-Nearest Neighbor from kknn package}
#' 		\item{\code{\linkS4class{lda}}}{Linear Discriminant Analysis from MASS package}
#' 		\item{\code{\linkS4class{logreg}}}{Logistic Regression from stats package}
#' 		\item{\code{\linkS4class{mda}}}{Mixture Discriminant Analysis from mda package}
#' 		\item{\code{\linkS4class{naiveBayes}}}{Naive Bayes from e1071 package}
#' 		\item{\code{\linkS4class{qda}}}{Quadratic Discriminant Analysis from MASS package}
#' 		\item{\code{\linkS4class{randomForest.classif}}}{Random Forest from randomForest package}
#' 		\item{\code{\linkS4class{rda}}}{Regularized Discriminant Analysis from klaR package}
#' 		\item{\code{\linkS4class{rpart.classif}}}{Decision Tree from rpart package}
#' 		\item{\code{\linkS4class{kernlab.svm.classif}}}{Support Vector Machines from kernlab package}  
#' }
#' 
#' @param target [\code{\link{character}}] \cr
#'  	  Name of the target variable.
#' @param data [\code{\link{data.frame}}] \cr 	
#'        A data frame containing the variables in the model.
#' @param weights [\code{\link{numeric}}] \cr 	
#'        An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @param costs [\code{\link{matrix}}] \cr 	
#'        A optional matrix of misclassification costs to be used in the fitting process. Default is zero-one loss.
#' @param type [\code{\link{character}}] \cr 	
#' 	      Specifies the type of the predictions - either probabilities ("prob") or classes ("class"). Default is "class".
#' 
#' 
#' 
#' @return An object of class \code{\linkS4class{classif.task}}.
#' 
#' @export
#' @rdname make.classif.task
#' 
#' @usage make.classif.task(target, data, weights, type)
#'
#' @examples
#' data(iris) 
#' # define a classification task for a decision tree (rpart) for the data set iris
#' ct <- make.classif.task("rpart.classif", data = iris, target = "Species")
#' 
#' @seealso \code{\linkS4class{classif.task}}, \code{\link{train}}, \code{\link{predict}}
#'  
#' @title make.classif.task


setMethod(
		f = "make.classif.task",
		signature = signature(
				target = "character",
				formula = "missing",
				data = "data.frame", 
				weights = "numeric", 
				costs = "matrix", 
				type = "character"
		),
		
		def = function(target, data, weights, costs, type) {
			ct <- new("classif.task", target=target, data=data, weights=weights, costs=costs, type=type)
			return(ct)
		}
)

setMethod(
		f = "make.classif.task",
		signature = signature(
				target = "missing",
				formula = "formula",
				data = "data.frame", 
				weights = "numeric", 
				costs = "matrix", 
				type = "character"
		),
		
		def = function(formula, data, weights, costs, type) {
			data2 <- model.frame(formula, data=data)
			target <- as.character(formula)[2]
			ct <- new("classif.task", target=target, data=data2, weights=weights, costs=costs, type=type)
			return(ct)
		}
)




