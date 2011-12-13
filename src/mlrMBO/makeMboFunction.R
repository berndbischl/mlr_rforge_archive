#' Transforms a target/fitness function, which takes a vector as its first argument, into
#' a function which takes a list of values (as required by \code{\link{mbo}}.   
#'
#' @title Create fitness function for mbo.
#' @param fun [\code{function}]\cr 
#'   Fitness function.
#' @return Function which accepts decision values as a list.
#' @export 
makeMboFunction = function(fun) {
  force(fun)
  function(x, ...) {
    fun(unlist(x), ...)
  }
}
