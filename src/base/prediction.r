setClass(
		"prediction",
		representation = representation(
				data.desc = "data.desc",
				task.desc = "task.desc",
				id = "ANY",
				response = "ANY",
				target = "ANY",
				weights = "ANY",
				prob = "ANY",
				decision = "ANY"
		)
)


setMethod(
		f = "[",
		signature = signature("prediction"),
		def = function(x,i,j,...,drop) {
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
			ii = x["id"]
			rr = x["response"]
			tt = x["target"]
			pp = x["prob"]
			dd = x["decision"]
			mm = data.frame(id=ii, response=rr)
			if (!is.null(tt))
				mm = cbind(mm, target=tt)
			if (!is.null(pp))
				mm = cbind(mm, prob=pp)
			if (!is.null(dd))
				mm = cbind(mm, decision=dd)
			return(mm)
		}
)


c.prediction = function(...) {
	preds = list(...)
	id = Reduce(c, lapply(preds, function(x) x@id))
	response = Reduce(c, lapply(preds, function(x) x@response))
	target = Reduce(c, lapply(preds, function(x) x@target))
	weights = Reduce(c, lapply(preds, function(x) x@weights))
	prob = Reduce(rbind, lapply(preds, function(x) x@prob))
	decision = Reduce(rbind, lapply(preds, function(x) x@decision))
	return(new("prediction", data.desc=preds[[1]]@data.desc, task.desc=preds[[1]]@task.desc, id=id, response=response, target=target, weights=weights, prob=prob, decision=decision));
}
