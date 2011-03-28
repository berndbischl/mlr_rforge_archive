# internal function to check whether costs are in correct format
# matrix with 0 rows/cols = no costs 
check.costs = function(costs, levs) {
  if (!is.matrix(costs) || mode(costs) != "numeric")
    stop("Costs have to be a numerical matrix!")
  if (!all(dim(costs) == 0)) {
    if (any(dim(costs) != length(levs)))
      stop("Dimensions of costs have to be the same as number of es!")
    rns = rownames(costs)
    cns = colnames(costs)
    if (!setequal(rns, levs) || !setequal(cns, levs))
      stop("Row and column names of cost matrix have to equal class levels!")
  }			
}