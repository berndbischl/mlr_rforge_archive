#' @include resample.instance.r
#' @include holdout.desc.r
roxygen()


#' Instance for hold-out resampling. 
#' 
#' @note Normally you don't want to use the S4 constructor, but instead \code{\link{make.res.instance}}.
#' 
#' @exportClass holdout.instance

setClass(
		"holdout.instance", 
		contains = c("subsample.instance")
)                                                     


#' Constructor.
#' @title holdout.instance constructor

setMethod(
		f = "initialize",
		signature = signature("holdout.instance"),
		def = function(.Object, desc, size) {
			callNextMethod(.Object, desc=desc, size=size)
		}
)

