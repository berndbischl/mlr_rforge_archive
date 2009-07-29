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

#' make.cv.instance generates training and test set indices for cross-validation. 
#' 
#' @param size [integer] \cr Size of the data set to resample. 
#' @param iters [integer] \cr Number of generated subsets / resampling iterations.
#' 
#' @return A \code{\linkS4class{cv.instance}} object, which encapsulates the generated indices of training and test sets.
#' 
#' @export
#' 
#' @examples 
#' data(iris)
#' rin <- make.cv.instance(iters=10, size=nrow(iris))
#' 
#' @seealso \code{\link{resample.fit}}
#' 
#' @title make.cv.instance

setMethod(
		f = "make.cv.instance",
		signature = c(size="numeric", iters="numeric"),
		def = function(size, iters) {
			desc <- new("cv.desc", iters=iters)
			return(new("cv.instance", desc=desc, size=size))
		}
)

