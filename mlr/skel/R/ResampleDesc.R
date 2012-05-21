# FIXME: remove iters from some desc. we also can use length of train.inds in instance
#FIXME: include complex example with repcv / b632+!
# FIXME startification in all
# B632+

#' Create a description object for a resampling strategy.
#' 
#' A description of a resampling algorithm contains all necessary information to 
#' create a \code{\link{ResampleInstance}}, when given the size of the data set.
#' For construction simply use the factory method \code{\link{makeResampleDesc}}. 
#' 
#' Object slots:
#' \describe{
#' \item{id [\code{character(1)}]}{Name of resampling strategy.}
#' \item{instance.class [\code{character(1)}]}{Class name of the corresponding \code{\link{ResampleInstance}}.}
#' \item{iters [\code{character(1)}]}{Number of iterations. Note that this is always the complete number of generated train/test sets, so for a 10 times repeated 5fold cross-validation it would be 50. }
#' \item{predict [\code{character(1)}]}{What to predict during resampling: 'train', 'test' or 'both' sets.}
#' }
#'  
#' Repeated cross-validation: Use \dQuote{RepCV}. Then you have to set the aggregation function for your preferred performance measure to 
#'   \dQuote{testgroup.mean} via \code{\link{setAggregation}}.
#' B632 bootstrap: Use \dQuote{BS} for bootstrap and set predict to \dQuote{both}. 
#'   Then you have to set the aggregation function for your preferred performance measure to 
#'   \dQuote{b632} via \code{\link{setAggregation}}.
#' B632+ bootstrap: Use \dQuote{BS} for bootstrap and set predict to \dQuote{both}. 
#'   Then you have to set the aggregation function for your preferred performance measure to 
#'   \dQuote{b632plus} via \code{\link{setAggregation}}.
#' 
#' @param method [\code{character(1)}]\cr
#'   \dQuote{CV} for cross-validation, \dQuote{LOO} for leave-one-out, \dQuote{StratCV} for stratified cross-validation, \dQuote{RepCV} for repeated cross-validation,\cr
#'   \dQuote{BS} for out-of-bag bootstrap, \dQuote{Subsample} for subsampling, \dQuote{Holdout} for holdout.	
#' @param iters [\code{integer(1)}]\cr
#'   Number of resampling iterations. Ignored for \dQuote{Holdout} and \dQuote{LOO}. 
#'   Default is 10.	 			
#' @param predict [\code{character(1)}]\cr
#'   What to predict during resampling: \dQuote{train}, \dQuote{test} or \dQuote{both} sets. Default is \dQuote{test}.
#' @param ... [any] \cr
#'		Further parameters for strategies.\cr 
#'			split [\code{numeric(1)}]: Proportion of training cases for \dQuote{Holdout} and \dQuote{Subsample} from between 0 and 1. Default is 2/3.\cr
#'			reps [integer(1)]: Repeats for \dQuote{RepCV}. Here \code{iters = folds * reps}. Default is 2. \cr
#'			folds [integer(1)]: Folds in the repeated CV for \code{RepCV}. Here \code{iters = folds * reps}. Default is 5. 
#' @return \code{\link{ResampleDesc}}.
#' @export
makeResampleDesc = function(method, iters=10L, predict="test", ...) {
  checkArg(method, choices=c("Holdout", "CV", "LOO", "Subsample", "Boostrap", "StratCV", "RepCV"))    
  iters = convertInteger(iters)
  checkArg(iters, "integer", len=1, na.ok=FALSE)    
  checkArg(predict, "character", choices=c("train", "test", "both"))    
  method = paste("makeResampleDesc", method, sep="")
  args = c(iters=iters, list(...))
  d = do.call(method, args)
  d$predict = predict
  return(d)
}


makeResampleDescInternal = function(id, instance.class, iters, predict="test") {
  structure(list(
    id = id,
    instance.class = instance.class,
    iters = iters,
    predict = predict
  ), class = "ResampleDesc")
}

print.ResampleDesc = function(x, ...) { 
  catf("%s with %i iterations.", x$id, x$iters)
  catf("Predict: %s", x$predict)
}

