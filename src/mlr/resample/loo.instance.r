#' @include ResampleInstance.R
#' @include CVDesc.R
roxygen()

setClass(
  "loo.instance", 
  contains = c("ResampleInstance")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("loo.instance"),
  def = function(.Object, desc, size, task) {
    desc@iters = size
    callNextMethod(.Object, desc=desc, size=size, test.inds=as.list(1:size))
  }
)
