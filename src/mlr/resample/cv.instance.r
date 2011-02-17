#' @include ResampleInstance.R
#' @include CVDesc.r
roxygen()



setClass(
		"cv.instance", 
		contains = c("ResampleInstance.nonseq")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("cv.instance"),
  def = function(.Object, desc, size, task) {
    inds <- sample(1:size)
    # don't warn when we can't split evenly
    test.inds = suppressWarnings(split(1:size, 1:desc@iters))
	callNextMethod(.Object, desc=desc, size=size, test.inds=test.inds)
  }
)


