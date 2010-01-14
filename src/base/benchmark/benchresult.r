
setClass(
		"bench.result",                                                     
		representation = representation(
				perf = "matrix", 
				tuned.pars = "list", 
				conf.mats = "list",
				resampling = "resample.instance"
		)
)




#' Getter.
#' @param x bench.result object
#' @param i [character]
#' \describe{
#'   \item{perf}{Performance matrix.}
#'   \item{tuned.pars}{Values of tuned paramters. NA if no tuning was done.}
#'   \item{conf.mats}{Confusion matrices - only for classification.}
#' }
#' @param j [integer or character] \cr Select subset of learners.
#' 
#' @rdname getter,learn.task-method
#' @aliases learn.task.getter getter,learn.task-method
#' @title Getter for learn.task

setMethod(
		f = "[",
		signature = signature("bench.result"),
		def = function(x,i,j,...,drop) {
			if (i == "perf"){
				if (missing(j))
					j = 1:ncol(perf)
				return(x@perf[,j])
			}
			if (i == "tuned.pars"){
				if (missing(j))
					j = 1:ncol(x@perf)
				if (length(j) == 1)
					return(x@tuned.pars[[j]])
				else
					return(x@tuned.pars[j])
			}
			if (i == "conf.mats"){
				if (missing(j))
					j = 1:ncol(x@perf)
				if (length(j) == 1)
					return(x@conf.mats[[j]])
				else
					return(x@conf.mats[j])
			}
			
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)





#' Conversion to string.

setMethod(
		f = "to.string",
		signature = signature("bench.result"),
		def = function(x) {
			ms = paste(capture.output(colMeans(x@perf)), collapse="\n")
			return(
					
					paste( 
							"Benchmark result\n",
							"Mean values:\n",
							ms, "\n",
							sep=""
					)
			)
		}
)


#' Prints the object by calling as.character.

setMethod(
		f = "print",
		signature = signature("bench.result"),
		def = function(x, ...) {
			cat(to.string(x))
		}
)

#' Shows the object by calling as.character.

setMethod(
		f = "show",
		signature = signature("bench.result"),
		def = function(object) {
			cat(to.string(object))
		}
)