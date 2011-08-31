#' Set how this measure will be aggregated after resampling. 
#' 
#' Possible are: test.mean, test.sd, test.median, test.min, test.max
#' They all calculate the respective statistic of the test set performances. 
#' 
#' @param measure [\code{\linkS4class{Measure}}]\cr 
#'        Performance measure.   
#' @param aggr [\code{\linkS4class{Aggregation}}] \cr
#'        Aggregation function.
#'        
#' @return \code{\linkS4class{Measure}} with changed aggregation behaviour.
#' @exportMethod setAggregation
#' @title Set aggregation function of measure. 
#' @rdname setAggregation 

setGeneric(
  name = "setAggregation",
  def = function(measure, aggr) {
    standardGeneric("setAggregation")
  }
)

#' @rdname setAggregation 
setMethod(
  f = "setAggregation",
  
  signature = signature(
    measure="Measure", 
    aggr="Aggregation" 
  ),
  
  def = function(measure, aggr) {
    measure@aggr = aggr
    return(measure)
  } 
)


