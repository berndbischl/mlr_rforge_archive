#' @include task.learn.r
#' @include resample.instance.r
roxygen()

#' export
setClass(
		"resample.result",
		representation = representation(
				task.desc="task.desc", 
				data.desc="data.desc", 
				instance="resample.instance", 
				preds="list", 
				extracted="list"
		)
)

#' Conversion to string.
setMethod(
		f = "to.string",
		signature = signature("resample.result"),
		def = function(x) {
			return(
					paste(
							"Resampling result for ", x@instance["name"], " with ", x["iters"], " iterations\n",
							#"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
							paste(capture.output(str(x@preds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)

#' Prints the object by calling as.character.
setMethod(
		f = "print",
		signature = signature("resample.result"),
		def = function(x, ...) {
			cat(to.string(x))
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("resample.result"),
		def = function(object) {
			cat(to.string(object))
		}
)


setMethod(
		f = "[",
		signature = signature("resample.result"),
		def = function(x,i,j,...,drop) {
			if (i == "iters")
				return(length(x@preds))
			
			if (i == "response") {
				if (missing(j)) {
					return(x["response", 1:x["iters"]])
				} else if(length(j) == 1) {
					return(x@preds[[j]]["response"])
				}
				else {
					return(lapply(j, function(k) x["response", k]))
				}
			}
			if (i == "all") {
				return(as.data.frame(x))
			}
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)




setMethod(
		f = "as.data.frame",
		signature = signature("resample.result"),
		def = function(x, row.names = NULL, optional = FALSE,...) {
			xs = lapply(x@preds, as.data.frame)
			xs = Reduce(rbind, xs)
			return(xs)
		}
)


