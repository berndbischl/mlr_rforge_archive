
##' @export 
#
#
#setClass("prediction",
#		representation(fitted="ANY")
#)
#
#setClass(
#		"p.regr",
#		contains = "prediction"
#)
#
#setClass(
#  "p.class",
#  contains = "prediction",
#  representation(probs="matrix")
#)
#
#
##----------------- constructor ---------------------------------------------------------
#
#
#make.prediction <- function(fitted) {
#	if (is.numeric(fitted))
#		return(new("p.regr", fitted=fitted))
#	if (is.factor(fitted))
#		return(new("p.class", classes=fitted))
#}
#
#
#setMethod(
#		f = "initialize",
#		signature = "prediction",
#		def = function(.Object, fitted) {
#			.Object@fitted = fitted
#			return(.Object)
#		}
#)
#
#
#
#setMethod(
#  f = "initialize",
#  signature = "p.class",
#  def = function(.Object, learn.task, classes=NULL, probs=NULL) {
#	  if (is.null(probs)) {
#		  .Object@probs = matrix(nrow=0, ncol=0)		
#		  .Object@fitted <- classes
#	} else {
#		.Object@probs <- probs
#		cn = colnames(probs)
#		classes = apply(probs,1, function(x) cn[which.max(x)])
#		.Object@fitted = factor(classes, levels=ct["class.levels"])
#	}
#	return(.Object)
#  }
#)
#
#setMethod(
#		f = "initialize",
#		signature = "p.regr",
#		def = function(.Object, fitted) {
#			callNextMethod(.Object, fitted)
#		}
#)


#----------------- accuracy ---------------------------------------------------------



setGeneric(
		name = "performance",
		def = function(true.y, pred.y, weights, measure, costs) {
			if(missing(weights)) {
				weights <- rep(1, length(true.y))
			}
			if(missing(measure)) {
				if (is.factor(true.y))
					measure <- make.measure("mmce")
				if (is.numeric(true.y))
					measure <- make.measure("mse")
			}
			if(missing(costs))
				costs <- NULL 
			standardGeneric("performance")
		}
)

#' \code{performance} measures the quality of predictions w.r.t. some loss function.
#' 
#' There are some more loss functions which you can use as performance measure, they are as follows:
#' For classification:
#' \itemize{ 
#' 		\item{\code{smce}}{Summed misclassification error}
#' } 
#' 
#' For regression:
#' \itemize{ 
#' 		\item{\code{sae}}{Sum of absolute errors}
#' 		\item{\code{mae}}{Median of absolute errors}
#' 		\item{\code{sse}}{Squared sum of errors}
#' 		\item{\code{mse}}{Mean squared error}
#' } 
#' 
#' @param	true.y [\code{\link{any}}] \cr
#' 			The data sets true labels.
#' @param 	pred.y [\code{\link{any}}] \cr
#' 			The predicted labels.
#' @param 	weights [\code{\link{numeric}}] \cr
#' 			An optional vector of weights to be used. Default is a weight of 1 for every case.
#' @param 	measure [\code{\link{list}}/ \code{\link{character}}] \cr
#' 			A list or a character which defines the loss function. Default is the mean misclassification error 
#' 			("mmce") for classification tasks and the mean squared error ("mse") for regression tasks. See details
#' 			for other loss functions.
#' @param 	costs [\code{\link{any}}] \cr
#' 			An optional possibility to specify costs, default is \code{NULL}.
#' 
#' @export
#' @rdname performance
#' 
#' @usage performance(true.y, pred.y, weights, measure, costs)
#'
#' @examples
#' data(iris) 
#' # define a classification task for a decision tree (rpart) for the data set iris
#' ct <- make.classif.task("rpart.classif", data=iris, formula=Species~.)
#' # specify train and test set indices
#' train.set <- seq(from=1L, to=150L, by=2L)
#' test.set <- seq(from=2L, to=150L, by=2L)
#' # train a model with the train set
#' m <- train(ct, subset=train.set) 
#' # predict the class of the test set measures 
#' preds <- predict(m, newdata=iris[test.set,])
#' # evaluate the predictions, first with mean misclassification error as loss function (default)
#' performance(true.y = iris[test.set, "Species"], pred.y = preds)		
#' # and with summed misclassification error
#' performance(true.y = iris[test.set, "Species"], pred.y = preds, measure="smce")
#' 
#' @title performance




setMethod(
		f = "performance",
		signature = signature(true.y="ANY", pred.y="ANY", weights="numeric", measure="list", costs="ANY"),
		def = function(true.y, pred.y, weights, measure, costs) {
			return(measure$fun(true.y, pred.y, weights))
		}
)

setMethod(
		f = "performance",
		signature = signature(true.y="ANY", pred.y="ANY", weights="numeric", measure="character", costs="ANY"),
		def = function(true.y, pred.y, weights, measure, costs) {
			return(make.measure(measure)$fun(true.y, pred.y, weights))
		}
)



