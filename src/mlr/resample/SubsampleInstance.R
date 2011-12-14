#' @include ResampleInstance.R
#' @include SubsampleDesc.R
roxygen()


setClass(
		"SubsampleInstance", 
		contains = c("ResampleInstance")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("SubsampleInstance"),
		def = function(.Object, desc, size, task) {
			if (missing(desc))
				return(.Object)
			inds = lapply(1:desc@iters, function(x) sample(1:size, size*desc@split))
			callNextMethod(.Object, desc=desc, size=size, train.inds=inds)
		}
)

