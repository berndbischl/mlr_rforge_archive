


setGeneric(
		name = "get.train.set",
		def = function(x, i) {
			standardGeneric("get.train.set")
		}
)


setGeneric(
		name = "get.test.set",
		def = function(x, i) {
			standardGeneric("get.test.set")
		}
)


setMethod(
		f = "get.train.set",
		signature = signature("resample.instance", "integer"),
		def = function(x, i) {
			return(x@inds[[i]])
		}
)

setMethod(
		f = "get.test.set",
		signature = signature("resample.instance", "integer"),
		def = function(x, i) {
			setdiff(1:x["size"], x@inds[[i]])
		}
)

setGeneric(
		name = "resample.update",
		def = function(x, task, model, pred) {
			standardGeneric("resample.update")
		}
)



setMethod(
		f = "resample.update",
		signature = signature("resample.instance", "learn.task", "wrapped.model", "prediction"),
		def = function(x, task, model, pred) {
			return(x)
		}
)

setGeneric(
		name = "resample.done",
		def = function(x, task, model, pred) {
			standardGeneric("resample.done")
		}
)



setMethod(
		f = "resample.done",
		signature = signature("resample.instance"),
		def = function(x, task, model, pred) {
			return(x)
		}
)


