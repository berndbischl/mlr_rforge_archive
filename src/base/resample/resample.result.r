#' @include task.learn.r
#' @include resample.instance.r
roxygen()

#' @exportClass resample.prediction

setClass(
		"resample.prediction",
		contains = c("prediction"),
		representation = representation(
				iter = "integer",
				instance="resample.instance", 
				extracted="list"
		)
)


setMethod(
		f = "initialize",
		signature = signature("resample.prediction"),
		def = function(.Object, task.desc, data.desc, instance, preds, extracted) {
			.Object@instance = instance
			.Object@extracted = extracted
			es = sapply(preds, function(x) length(x@response))
			.Object@iter = rep(1:length(preds), times=es)
			p = Reduce(c, preds)
			p1 = preds[[1]]
			callNextMethod(.Object, task.desc=p1@task.desc, data.desc=p1@data.desc, id=p@id, response=p@response, prob=p@prob, decision=p@decision, target=p@target, weights=p@weights)
		}
)



#' Conversion to string.
setMethod(
		f = "to.string",
		signature = signature("resample.prediction"),
		def = function(x) {
			return(
					paste(
							"Resampling result for ", x@instance["name"], " with ", x["iters"], " iterations\n",
							#"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
							#paste(capture.output(str(x@preds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)

#' Prints the object by calling as.character.
setMethod(
		f = "print",
		signature = signature("resample.prediction"),
		def = function(x, ...) {
			cat(to.string(x))
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("resample.prediction"),
		def = function(object) {
			cat(to.string(object))
		}
)


setMethod(
		f = "[",
		signature = signature("resample.prediction"),
		def = function(x,i,j,...,drop) {
			if (i == "iters")
				return(x@instance["iters"])
			
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)


setMethod(
		f = "as.list",
		signature = signature("resample.prediction"),
		def = function(x, all.names = FALSE, ...) {
			preds = list()
			for (i in 1:x@instance["iters"]) {
				j = which(x@iter == i)
				preds[[i]] = new("prediction", task.desc=x@task.desc, data.desc=x@data.desc, id=x@id[j], response=x@response[j], prob=x@prob[j,], decision=x@decision[j,], target=x@target[j], weights=x@weights[j])
			}
			return(preds)
		}
)


