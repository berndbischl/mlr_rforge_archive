#' Result from \code{\link{predict}}.    
#' 
#' @exportClass prediction
#' @title Prediction.
#' @seealso \code{\link{performance}}


setClass(
		"prediction",
		representation = representation(
				df = "data.frame",
				data.desc = "data.desc",
				task.desc = "task.desc",
				time.train = "numeric",
				time.predict = "numeric"
		)
)


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


make.prediction = function(data.desc, task.desc, id, truth, response, weights, prob, decision, time.train, time.predict) {
	xs = list()
	# if null no col in df present
	xs[["id"]] = id
	xs[["response"]] = response
	xs[["truth"]] = truth
	xs[["weights"]] = weights
	xs[["prob"]] = prob
	xs[["decision"]] = decision
	df = as.data.frame(xs)
	new("prediction", data.desc, task.desc, df, time.train, time.predict)
}



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


setMethod(
		f = "as.data.frame",
		signature = signature("prediction"),
		def = function(x, row.names = NULL, optional = FALSE,...) {
			return(df)
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
