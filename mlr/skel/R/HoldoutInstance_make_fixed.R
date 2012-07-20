#' Generate a fixed holdout instance for resampling. 
#' 
#' @param train.inds [\code{integer}]\cr
#'   Indices for training set.
#' @param test.inds [\code{integer}]\cr
#'   Indices for test set.
#' @param size [\code{integer(1)}]\cr
#'   Size of the data set to resample.
#' @return [\code{\link{ResampleInstance}}].
#' @export 
makeFixedHoldoutInstance = function(train.inds, test.inds, size) {
  train.inds = convertIntegers(train.inds)  
  test.inds = convertIntegers(test.inds)  
  size = convertInteger(size)
  test.inds = convertIntegers(test.inds)  
  checkArg(train.inds, "integer", na.ok=FALSE)
  checkArg(test.inds, "integer", na.ok=FALSE)
  checkArg(size, "integer", len=1, na.ok=FALSE)
  rdesc = makeResampleDesc("Holdout", split=length(train.inds)/size)
  rin = makeResampleInstance(rdesc, size=size)
  rin$train.inds[[1]] = train.inds
  rin$test.inds[[1]] = test.inds
  return(rin)
}
