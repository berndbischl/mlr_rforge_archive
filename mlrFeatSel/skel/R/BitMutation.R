#' Bit Mutation
#' 
#' Takes a bit string and flips its elements based on a given mutation rate. 
#' 
#' @param x [\code{logical}]\cr 
#'   A bit string, which should be mutated.  
#' @param rate [\code{numeric(1)}]\cr   
#'   A number representing the probability of flipping an element of the bit string. Hence it 
#'   should be between 0 and 1. Default is \code{1 / length(x)}.
#' @return [\code{\link{BitMutation}}].
#' @name BitMutation
#' @rdname BitMutation
#' @aliases BitMutation
#' @examples
#' x <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
#' flip <- BitMutation(x, 1)
#' table(x, flip)
#' ## All the labels are flipped.
#' 
#' flip2 <- BitMutation(x)
#' table(x, flip2)
#' ## The elements of x are flipped with a probability of 1/length(x).
#' 
#' flip3 <- BitMutation(x, rate = 0.05)
#' table(x, flip3)
#' ## The elements of x are flipped with a probability of 5%.
NULL

BitMutation = function(x, rate = 1/length(x)) {
  n = length(x)
  flip = rbinom(n, 1, rate)
  x = (x + flip) %% 2
  return(x)
}