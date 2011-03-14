
#' Creates a data.frame of new features
#'
#' @param data [data.frame]\cr 
#'   Data to summarize. Columns can be of type numeric, integer, logical, factor or character. 
#'   Characters and logicals will be treated as factors.   
#' @param vars [\code{character}]\cr 
#'   Names of (numeric) features to create monomials from.
#'   Per default all numeric columns in \code{data} are used. 
#' @return [\code{data.frame}], which contains the derived features as columns. 
#' 
#' @export
#' @title Summarize a data.frame.

makeMonomials = function(data, vars) {
  s1 = paste(colnames(vars), collapse=",")
  s2 = sprintf("TWI(%s)+PQ(%s)-1", s1)
  m = model.matrix(as.formula(s2), data)
}