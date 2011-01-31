#' @include resample.instance.r
#' @include cv.desc.r
roxygen()

setClass(
  "loo.instance", 
  contains = c("resample.instance.nonseq")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("loo.instance"),
  def = function(.Object, desc, size, task) {
    callNextMethod(.Object, desc=desc, size=size, test.inds=as.list(1:size))
  }
)
