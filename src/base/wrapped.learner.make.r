#' Create learner object. 
#' 
#' @param name [string] \cr
#'   Name of learner.
#' 
#' @return \code{\linkS4class{wrapped.learner}}.
#' 
#' @export
#' 
make.learner = function(name, task, ...) {
	parset = list(...)
	name2 = NULL
	if (!missing(task)) {
		if (is(task, "classif.task")) {
			if (!extends(name, "wrapped.learner.classif")) {
				name = paste(name, "classif", sep=".")
				if (extends(name, "wrapped.learner.classif"))
					name2 = name
			} else {
				name2 = name
			}
		} else if (is(task, "regr.task")){
			if (!extends(name, "wrapped.learner.regr")) {
				n = paste(name, "regr", sep=".")
				if (extends(name, "wrapped.learner.regr"))
					name2 = name
			} else {
				name2 = name
			}
		}
	} else {
		if (extends(name, "wrapped.learner"))
			name2 = name
	}
	if (!is.null(name2))
		return(new(name2, parset=parset))
	else 
		stop("Cannot find corresponding learner class for name: ", name)
}





#' Creates a wrapped learner object.  
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
#' @param name [\code{\link{character}}] \cr
#'  	  Name of the learning algorithm.
#' @param task [\code{\link{character}}] \cr
#'  	  Name of the learning algorithm.
#' 
#' @return An object of class \code{\linkS4class{wrapped.learner}}.
#' 
#' @export
#' @rdname make.learner
#' 
#' @usage make.learner(name, task)
#'  
#' @title make.learner



