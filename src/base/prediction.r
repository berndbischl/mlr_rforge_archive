#' Result from \code{\link{predict}}. 
#' Use \code{as.data.frame} to access all information in a convenient format.   
#' 
#' Getter.\cr
#' 
#' \describe{
#'	\item{negative [string]}{Negative class label for binary classification.}
#'  \item{id [numeric]}{Vector of index numbers of predicted cases from the task.}
#'  \item{response [numeric]}{ Predicted response values.}
#'  \item{truth}{True target values.}
#'  \item{prob [numeric | matrix]}{Predicted probabilities. If it's a binary problem only the probabilities for the postive class are returned.}
#'  \item{decision [matrix]}{Predicted decision values.}
#' }
#' 
#' @exportClass prediction
#' @title Prediction.
#' @seealso \code{\link{performance}}


setClass(
		"prediction",
		contains = c("object"),
		representation = representation(
				df = "data.frame",
				data.desc = "data.desc",
				task.desc = "task.desc",
				time.train = "numeric",
				time.predict = "numeric"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("prediction"),
		def = function(.Object, data.desc, task.desc, df, time.train, time.predict) {
			if (missing(df))
				return(.Object)
			.Object@df = df			
			.Object@data.desc = data.desc			
			.Object@task.desc = task.desc			
			.Object@time.train = time.train			
			.Object@time.predict = time.predict			
			return(.Object)
		}
)


make.prediction = function(data.desc, task.desc, id, truth, response, prob, decision, time.train, time.predict) {
	xs = list()
	# if null no col in df present
	xs[["id"]] = id
	xs[["response"]] = response
	xs[["truth"]] = truth
	xs[["prob"]] = prob
	xs[["decision"]] = decision
	df = as.data.frame(xs)
	new("prediction", data.desc, task.desc, df, time.train, time.predict)
}


#' Getter.
#' @rdname prediction-class


setMethod(
		f = "[",
		signature = signature("prediction"),
		def = function(x,i,j,...,drop) {
			if (i == "id")
				return(x@df$id)
			if (i == "response")
				return(x@df$response)
			if (i == "truth")
				return(x@df$truth)
			if (i == "prob") {
				cns = colnames(x@df)
				return(x@df[, grep("^prob", cns)])
			}
			if (i == "decision") {
				cns = colnames(x@df)
				return(x@df[, grep("^decision", cns)])
			}
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)

#'  Convert to data.frame
#' @rdname prediction-class 
#' @export
setMethod(
		f = "as.data.frame",
		signature = signature("prediction"),
		def = function(x, row.names = NULL, optional = FALSE,...) {
			return(x@df)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("prediction"),
		def = function(x) {
			return(
					paste(
							"Prediction\n",
							paste(capture.output(str(as.data.frame(x))), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)


#c.prediction = function(...) {
#	preds = list(...)
#	id = Reduce(c, lapply(preds, function(x) x@id))
#	response = Reduce(c, lapply(preds, function(x) x@response))
#	target = Reduce(c, lapply(preds, function(x) x@target))
#	weights = Reduce(c, lapply(preds, function(x) x@weights))
#	prob = Reduce(rbind, lapply(preds, function(x) x@prob))
#	decision = Reduce(rbind, lapply(preds, function(x) x@decision))
#	return(new("prediction", data.desc=preds[[1]]@data.desc, task.desc=preds[[1]]@task.desc, id=id, response=response, target=target, weights=weights, prob=prob, decision=decision));
#}
