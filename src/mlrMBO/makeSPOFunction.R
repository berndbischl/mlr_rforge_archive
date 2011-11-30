#' Transforms a target/fitness function, which takes a vector as its first argument, into
#' a function which takes a list of values (as required by \code{\link{spo}}.   
#'
#' @param fun [function]\cr 
#'   Fitness function.
#' @return Function which accepts decision values as a list.
#' @export 
#' @seealso \code{\link{makeSPOParamSet}} 
#' @title Create fitness function for SPO.
makeSPOFunction = function(fun) {
  force(fun)
  function(x, ...) {
    fun(unlist(x), ...)
  }
}
