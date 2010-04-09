#' @include resample.instance.r
#' @include cv.desc.r
roxygen()



setClass(
		"cv.instance", 
		contains = c("resample.instance")
)                                                     

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


