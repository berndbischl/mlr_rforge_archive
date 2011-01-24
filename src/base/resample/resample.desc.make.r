#todo: include complex example with repcv / b632+!
#' Generates a description object for a resampling strategy.
#' 
#' Repeated cross-validation: Use 'repcv'. Then you have to set the aggregation function for your preferred performance measure to 
#'   'testgroup.mean' via \code{\link{set.aggr}}.
#' B632 bootstrap: Use 'bs' for bootstrap and set predict to 'both'. 
#'   Then you have to set the aggregation function for your preferred performance measure to 
#'   'b632' via \code{\link{set.aggr}}.
#' B632+ bootstrap: Use 'bs' for bootstrap and set predict to 'both'. 
#'   Then you have to set the aggregation function for your preferred performance measure to 
#'   'b632plus' via \code{\link{set.aggr}}.
#' 
#' @param method [string] \cr
#'   'cv' for cross-validation, 'stratcv' for stratified cross-validation,  'repcv' for repeated cross-validation,\cr
#'   'bs' for out-of-bag bootstrap, 'subsample' for subsampling, 'holdout' for holdout.	
#' @param iters [integer] \cr
#'   Number of resampling iterations. Ignored for 'holdout'. Default is 10.	 			
#' @param predict [character] \cr
#'   What to predict during resampling: 'train', 'test' or 'both' sets. Default is 'test'.
#' @param ... [any] \cr
#'		Further parameters for strategies.\cr 
#'			split [numeric(1)]: Proportion of training cases for 'holdout' and 'subsample' from between 0 and 1. Default is 2/3.\cr
#'			reps [integer(1)]: Repeats for 'repcv'. Here 'iters' = 'folds' x 'reps'. Default is 10. \cr
#'			folds [integer(1)]: Folds in the repeated CV for 'repcv'. Here 'iters' = 'folds' x 'reps'. Default is 10. 
#' 
#' @return \code{\linkS4class{resample.desc}}.
#' @export 
#' @title Construct resampling description.


setGeneric(
  name = "make.res.desc",
  def = function(method, iters, predict, ...) {
    if (missing(iters))
      iters = 10L
    if (is.numeric(iters))
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
    if (!(length(predict) ==1 && predict %in% c("train", "test", "both")))
      stop("Argument predict can only be: 'train', 'test', 'both'!")
    cc = paste(method, "desc", sep=".")
    d = new(cc, iters=iters, ...)
    d@predict = predict
    return(d)
  }
)
