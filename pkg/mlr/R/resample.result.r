#' @include task.learn.r
#' @include resample.instance.r
roxygen()

setClass(
		"resample.result",
		representation(ri.class="character", ri.name="character", preds="list", models="list")
)


setMethod(
		f = "as.character",
		signature = "resample.result",
		def = function(x) {
			return(
					paste(
							"Resampling result for ", x@ri.name, " with ", x["iters"], " iterations\n",
							"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
							paste(capture.output(str(x@preds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)


setMethod(
		f = "print",
		signature = "resample.result",
		def = function(x, ...) {
			cat(as.character(x))
		}
)

setMethod(
		f = "show",
		signature = "resample.result",
		def = function(object) {
			cat(as.character(object))
		}
)







#----------------- getter ---------------------------------------------------------


setMethod(
		f = "[",
		signature = "resample.result",
		def = function(x,i,j,...,drop) {
			if (i == "iters")
				return(length(x@preds))
			
			if (i == "fitted") {
				if (missing(j)) {
					return(x["fitted", 1:x["iters"]])
				} else if(length(j) == 1) {
					return(x@preds[[j]])
				}
				else {
					return(lapply(j, function(k) x["fitted", k]))
				}
			}
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)


