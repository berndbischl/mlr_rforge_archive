#' @include resample.instance.r
#' @include cv.desc.r
roxygen()


#' A cv.instance object is mainly a set of integer vectors generated by cross-validation. 
#' They indicate the training part of data for different instances of cv. 
#'
#' @note Normally you don't want to use the S4 constructor, but instead \code{\link{make.cv.instance}}.
#' 
#' @title cv.instance
#' @exportClass cv.instance

setClass(
		"cv.instance", 
		contains = c("resample.instance")
)                                                     

#' Constructor.
#' @title cv.instance constructor

setMethod(
  f = "initialize",
  signature = signature("cv.instance"),
  def = function(.Object, desc, size) {
    inds <- sample(1:size)
    # don't warn when we can't split evenly
    s <- suppressWarnings(split(1:size, 1:desc["iters"]))
    inds <- lapply(s, function(x) inds[-x])

	callNextMethod(.Object, desc=desc, size=size, inds=inds)
  }
)


