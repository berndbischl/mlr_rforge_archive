#' @include task.classif.r
roxygen()

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
#' 		\item{\code{\linkS4class{adaboost}}}{ Boosting from adabag package}
#' 		\item{\code{\linkS4class{kknn.classif}}}{ k-Nearest Neighbor from kknn package}
#' 		\item{\code{\linkS4class{lda}}}{ Linear Discriminant Analysis from MASS package}
#' 		\item{\code{\linkS4class{logreg}}}{ Logistic Regression from stats package}
#' 		\item{\code{\linkS4class{mda}}}{ Mixture Discriminant Analysis from mda package}
#' 		\item{\code{\linkS4class{naiveBayes}}}{ Naive Bayes from e1071 package}
#' 		\item{\code{\linkS4class{qda}}}{ Quadratic Discriminant Analysis from MASS package}
#' 		\item{\code{\linkS4class{randomForest.classif}}}{ Random Forest from randomForest package}
#' 		\item{\code{\linkS4class{rda}}}{ Regularized Discriminant Analysis from klaR package}
#' 		\item{\code{\linkS4class{rpart.classif}}}{ Decision Tree from rpart package}
#' 		\item{\code{\linkS4class{kernlab.svm.classif}}}{ Support Vector Machines from kernlab package}  
#' }
#' 
#' @param name [\code{\link{character}}] \cr
#'   	  Name of task / data set to be used string representations later on. Default is empty string.
#' @param data [\code{\link{data.frame}}] \cr 	
#'        A data frame containing the variables in the model.
#' @param target [\code{\link{character}}] \cr
#'  	  Name of the target variable.
#' @param formula [\code{\link{formula}}] \cr
#'        Instead of specifying the target, you can use the formula interface. 
#'        If you are using just a subset of the variables of transformations of the variables, this will built a new internal 
#'        data frame by calling \code{\link{model.frame}}.
#' @param excluded [\code{\link{character}}]
#'        Names of inputs, which should be generally disregarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [\code{\link{numeric}}] \cr 	
#'        An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @param costs [\code{\link{matrix}}] \cr 	
#'        A optional matrix of misclassification costs to be used in the fitting process. Default is zero-one loss.
#' 
#' 
#' 
#' @return An object of class \code{\linkS4class{classif.task}}.
#' 
#' @export
#' @rdname make.classif.task
#' 
#' @usage make.classif.task(name, data, target, formula, excluded, weights, costs)
#'
#' @examples
#' data(iris) 
#' # define a classification task for iris data set
#' ct <- make.classif.task(data = iris, target = "Species")
#' 
#' @seealso \code{\linkS4class{classif.task}}
#'  
#' @title Contruct classification task


setGeneric(
		name = "make.classif.task",
		def = function(name, data, target, formula, excluded, weights, costs) {
			if(missing(name))
				name=""
			if (missing(excluded))
				excluded = character(0)
			if (missing(weights))
				weights <- rep(1, nrow(data))
			if (missing(costs)) {
				# we set costs in constructor after data preparation
				costs=matrix(0,0,0)
			}		
			standardGeneric("make.classif.task")
		}
)


#' @export

setMethod(
		f = "make.classif.task",
		signature = signature(
				name = "character",
				data = "data.frame", 
				target = "character",
				formula = "missing",
				excluded = "character",
				weights = "numeric", 
				costs = "matrix" 
			),
		
		def = function(name, data, target, excluded, weights, costs) {
			ct <- new("classif.task", name=name, target=target, data=data, excluded=excluded, weights=weights, costs=costs)
			return(ct)
		}
)

#' @export

setMethod(
		f = "make.classif.task",
		signature = signature(
				name = "character",
				data = "data.frame", 
				target = "missing",
				formula = "formula",
				excluded = "character",
				weights = "numeric", 
				costs = "matrix" 
		),
		
		def = function(name, data, formula, excluded, weights, costs) {
			data2 <- model.frame(formula, data=data)
			target <- as.character(formula)[2]
			ct <- new("classif.task", name=name, target=target, data=data2, excluded, weights=weights, costs=costs)
			return(ct)
		}
)




