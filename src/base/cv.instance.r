#' @include resample.instance.r
#' @include cv.desc.r
roxygen()

#' @export


setClass("cv.instance", contains="resample.instance")                                                     

setMethod(
  f = "initialize",
  signature = "cv.instance",
  def = function(.Object, desc, size) {
    inds <- sample(1:size)
    # don't warn when we can't split evenly
    s <- suppressWarnings(split(1:size, 1:desc["iters"]))
    inds <- lapply(s, function(x) inds[-x])

	callNextMethod(.Object, desc=desc, size=size, inds=inds)
  }
)


setGeneric(
	name = "make.cv.instance",
	def = function(size, iters) {
		standardGeneric("make.cv.instance")
	}
)

#' @export

setMethod(
		f = "make.cv.instance",
		signature = c(size="numeric", iters="numeric"),
		def = function(size, iters) {
			desc <- new("cv.desc", iters=iters)
			return(new("cv.instance", desc=desc, size=size))
		}
)

