#' Generates a description object for a resampling strategy.
#' 
#' repcv: Set aggregation function  on your preferred performance to measure to "repcv". 
#' B632: Use "bs" for bootstrap and set predict to "both", then set aggregation function 
#'   on your preferred performance to measure to "b632". 
#' B632+: Use "bs" for bootstrap and set predict to "both", then set aggregation function 
#'   on your preferred performance to measure to "b632plus". 
#' 
#' @param method [string] \cr
#'   "cv" for cross-validation, "stratcv" for stratified cross-validation,  "repcv" for repeated cross-validation,\cr
#'   "bs" for out-of-bag bootstrap, "subsample" for subsampling, "holdout" for holdout.	
#' @param iters [integer] \cr
#'   Number of resampling iterations. Not needed for "holdout". 	 			
#' @param predict [character] \cr
#'   What to predict during resampling: "train", "test" or "both" sets.
#' @param ... [any] \cr
#'		Further parameters for strategies.\cr 
#'			split: Percentage of training cases for "holdout", "subsample".\cr
#'			reps: Repeats for "repcv"
#' 
#' @return \code{\linkS4class{resample.desc}}.
#' @export 
#' @title Construct resampling description.


setGeneric(
  name = "make.res.desc",
  def = function(method, iters, predict, ...) {
    if (!missing(iters) && is.numeric(iters))
      iters = as.integer(iters)
    if (missing(predict))
      predict = "test"
    standardGeneric("make.res.desc")
  }
)

#' @export 
#' @rdname make.res.desc


setMethod(
  f = "make.res.desc",
  signature = c(method="character", iters="integer", predict="character"),
  def = function(method, iters, predict, ...) {
    cc = paste(method, "desc", sep=".")
    if (!missing(iters)) {
      iters = as.integer(iters)
      d = new(cc, iters=iters, predict=predict, ...)
    } else {
      d = new(cc, predict=predict, ...)
    }
    return(d)
  }
)
