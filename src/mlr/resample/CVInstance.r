#' @include ResampleInstance.R
#' @include CVDesc.R
roxygen()



setClass(
		"CVInstance", 
		contains = c("ResampleInstance")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("CVInstance"),
  def = function(.Object, desc, size, task) {
    test.inds = sample(1:size)
    # don't warn when we can't split evenly
    test.inds = suppressWarnings(split(test.inds, 1:desc@iters))
	  callNextMethod(.Object, desc=desc, size=size, test.inds=test.inds)
  }
)


