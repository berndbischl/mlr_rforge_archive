#' Get property of learner object. Currently available are:
#' \describe{
#'  \item{type [\code{character(1)}]}{Can be \dQuote{classif} or \dQuote{regr}.}
#'  \item{numerics [\code{logical(1)}]}{Can numeric features be handled?}
#'  \item{factors [\code{logical(1)}]}{Can factor features values be handled?}
#'  \item{missings [\code{logical(1)}]}{Can missing values be handled?}
#'  \item{weights [\code{logical(1)}]}{Can case weights be handled?}
#'  \item{oneclass [\code{logical(1)}]}{Can one-class problems be handled?}
#'  \item{twoclass [\code{logical(1)}]}{Can two-class problems be handled?}
#'  \item{multiclass [\code{logical(1)}]}{Can multi-class problems be handled?}
#'  \item{prob [\code{logical(1)}]}{Can probabilities be predicted?}
#' }
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner object.   
#' @param prop [\code{character(1)}] \cr
#'   Name of property.
#'        
#' @return Property values.
#' @exportMethod getProperty
#' @title Get property of learner object.
#' @seealso \code{\link{setProperty}}, \code{\link{setProperties}} 
#' @rdname getProperty 

setGeneric(
  name = "getProperty",
  def = function(learner, prop) {
    standardGeneric("getProperty")
  }
)
