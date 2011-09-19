#' Construct a ParameterSet for a numeric vector from lower / upper bounds.
#' Convenience function for \code{\link{spo}}.
#' 
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param lower [\code{numeric(1)}] \cr
#'   Lower bounds. Default is \code{-Inf}.
#' @param upper [\code{numeric(1)}] \cr
#'   Upper bounds. Default is \code{Inf}.
#' @return [{\linkS4class{ParameterSet}}]
#' @export 
#' @seealso \code{\link{makeSPOFunction}}
#' @title Create parameter set for SPO.
makeSPOParameterSet = function(id, lower=-Inf, upper=Inf) {
  if (length(lower) == 1)
    lower = rep(lower, length(upper))
  if (length(upper) == 1)
    upper = rep(upper, length(lower))
  if (length(lower) != length(upper))
    stop("Lower and upper bounds must have same length!")
  ps = Map(function(i,l,u) 
    makeNumericParameter(paste(id, i, sep=""), lower=l, upper=u), 
    1:length(lower), lower, upper
  )
  do.call(makeParameterSet, ps)
}
