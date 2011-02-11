#' Set how this measure will be aggregated after resampling. 
#' 
#' Possible are: test.mean, test.sd, test.median, test.min, test.max
#' They all calculate the respective statistic of the test set performances. 
#' 
#' @param measure [\code{\linkS4class{measure}}]\cr 
#'        Performance measure.   
#' @param aggrs [list of \code{\linkS4class{aggr}}] \cr
#'        Aggregation functions.
#'        
#' @return \code{\linkS4class{measure}} with changed aggregation behaviour.
#' @exportMethod set.aggr
#' @title Set aggregation functions of measure. 
#' @rdname set.aggr 

setGeneric(
  name = "set.aggr",
  def = function(measure, aggrs) {
    if (!is.list(aggrs))
      aggrs = list(aggrs)
    standardGeneric("set.aggr")
  }
)

#' @rdname set.aggr 
setMethod(
  f = "set.aggr",
  
  signature = signature(
    measure="measure", 
    aggr="list" 
  ),
  
  def = function(measure, aggrs) {
    measure@aggr = aggrs
    return(measure)
  } 
)


