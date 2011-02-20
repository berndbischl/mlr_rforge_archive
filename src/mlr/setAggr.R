#' Set how this measure will be aggregated after resampling. 
#' 
#' Possible are: test.mean, test.sd, test.median, test.min, test.max
#' They all calculate the respective statistic of the test set performances. 
#' 
#' @param measure [\code{\linkS4class{Measure}}]\cr 
#'        Performance measure.   
#' @param aggrs [list of \code{\linkS4class{aggr}}] \cr
#'        Aggregation functions.
#'        
#' @return \code{\linkS4class{Measure}} with changed aggregation behaviour.
#' @exportMethod setAggr
#' @title Set aggregation functions of measure. 
#' @rdname setAggr 

setGeneric(
  name = "setAggr",
  def = function(measure, aggrs) {
    if (!is.list(aggrs))
      aggrs = list(aggrs)
    standardGeneric("setAggr")
  }
)

#' @rdname setAggr 
setMethod(
  f = "setAggr",
  
  signature = signature(
    measure="Measure", 
    aggr="list" 
  ),
  
  def = function(measure, aggrs) {
    measure@aggr = aggrs
    return(measure)
  } 
)


