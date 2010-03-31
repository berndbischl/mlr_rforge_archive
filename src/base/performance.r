

setGeneric(
		name = "performance",
		def = function(x, measures, losses, aggr) {
			if (missing(measures))
				measures=default.measures(x@task.desc)
			measures = make.measures(measures)
			if (missing(losses))
				losses=list()
			losses = make.losses(losses)
			if(missing(aggr))
				aggr = default.aggr(x@task.desc)
			aggr = make.aggrs(aggr)
			standardGeneric("performance")
		}
)

#' \code{performance} measures the quality of predictions w.r.t. some loss function.
#' 
#' There are some more loss functions which you can use as performance measure, they are as follows:
#' For classification:
#' \describe{ 
#' 		\item{\code{smce}}{Summed misclassification error}
#' } 
#' 
#' For regression:
#' \describe{ 
#' 		\item{\code{sae}}{Sum of absolute errors}
#' 		\item{\code{mae}}{Median of absolute errors}
#' 		\item{\code{sse}}{Squared sum of errors}
#' 		\item{\code{mse}}{Mean squared error}
#' } 
#' 

#' @param	true.y [ANY] \cr
#' 			The data sets true labels.
#' @param 	pred.y [ANY] \cr
#' 			The predicted labels.
#' @param 	weights [\code{\link{numeric}}] \cr
#' 			An optional vector of weights to be used. Default is a weight of 1 for every case.
#' @param 	loss [\code{\linkS4class{loss}} or \code{\link{character}}] \cr
#' 			Loss function. Default is "zero-one" for classifictaion and "squared" for regression.
#' @param 	aggr [\code{\link{function}}] \cr
#' 			Function to aggregate individual loss values to a single one. Default is mean.
#' 
#' @return The performance.
#' 
#' @export
#' @rdname performance
#' 
#' @usage performance(true.y, pred.y, weights, loss, aggr)
#'
#' @examples
#' data(iris) 
#' ct <- make.classif.task(data=iris, target="Species")
#' # specify train and test set indices
#' train.set <- seq(from=1L, to=150L, by=2L)
#' test.set <- seq(from=2L, to=150L, by=2L)
#' # train a model with the train set
#' m <- train("rpart.classif", ct, subset=train.set) 
#' # predict the class of the test set measures 
#' preds <- predict(m, newdata=iris[test.set,])
#' # evaluate the predictions, first with mean misclassification error as loss function (default)
#' performance(true.y = iris[test.set, "Species"], pred.y = preds)		
#' 
#' @title performance

setMethod(
		f = "performance",
		signature = signature(x="prediction", measures="list", losses="list", aggr="list"),
		def = function(x, measures, losses, aggr) {
			td = x@task.desc
			dd = x@data.desc
			ms = sapply(measures, function(f) f(x))
			ls = lapply(losses, function(f) cbind(
						x@id,		
						f(x["target"], x["response"], x["weights"], td, dd)
			))
#			if(length(ms[[1]]) != 1)
#				stop("Measure has to return a scalar value!")
			ls = as.data.frame(Reduce(rbind, ls))
			g = function(x) {
				n = attr(x, "name")
				if (is.null(n)) 
					return(NA)
				else 
					return(n)
			}
			names(ms) = sapply(measures, g)
			if (length(losses) > 0)
				colnames(ls) = c("id", sapply(losses, g))
			
			if (length(losses) > 0)
				return(list(measures=ms, losses=ls))
			return(list(measures=ms))
		}
)



