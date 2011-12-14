#' @include ResampleInstance.R
#' @include CVDesc.R
roxygen()

setClass(
  "LOOInstance", 
  contains = c("ResampleInstance")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("LooInstance"),
  def = function(.Object, desc, size, task) {
    desc@iters = size
    callNextMethod(.Object, desc=desc, size=size, test.inds=as.list(1:size))
  }
)
