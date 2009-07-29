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
#' make.cv.instance generates indices which represent training and test sets. 
#' @param size[numeric] \cr With size the training plus test set size is specified, normally it is the number of examples in the dataset. 
#' @param iters [numeric] \cr Iters is the number of generated subsets. 
#' 
#' @return A list with the iters-number of training set indices is returned.
#' 
#' @example 
#' data(iris)
#' rin <- make.cv.instance(iters=10, size=nrow(iris))
#' 
#' @seealso \code{\linkS4class{resample.fit}}
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

