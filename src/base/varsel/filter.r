#' Filter features by using a (fast?) numerical importance criterion.
#' 
#' Look at package FSelector for details on the filter algorithms. 
#' 
#' @param x [\code{\linkS4class{LearnTask}}]\cr 
#'   Task to filter features from.   
#' @param method [string] \cr
#'   Filter method. Available are:
#'   linear.correlation, rank.correlation, information.gain, gain.ratio, symmetrical.uncertainty, chi.squared, random.forest.importance, relief, oneR
#' @param threshold [single double] \cr
#'   Only features whose importance value exceed this are selected.  
#'        
#' @return A list with the following entries:
#' \describe{
#'   \item{vars [character]}{Selected features.}
#'   \item{vals [named double]}{Importance values for all features used in filter.}
#' }
#' 
#' @exportMethod varfilter
#' @title Set aggregation functions of measure. 
#' @rdname varfilter 

setGeneric(
  name = "varfilter",
  def = function(task, method, threshold, ...) {
    match.arg(method, 
      c("linear.correlation", "rank.correlation", "information.gain", "gain.ratio", "symmetrical.uncertainty", 
        "chi.squared", "random.forest.importance", "relief", "oneR"))
    standardGeneric("varfilter")
  }
)

#' @rdname varfilter 
setMethod(
  f = "varfilter",
  
  signature = signature(task="LearnTask", method="character", threshold="numeric"),
  
  def = function(task, method, threshold, ...) {
    require.packs("FSelector")
    tn = task["target"]
    f = task["formula"]
    data = task["data"]
    if (method == "linear.correlation") 
      x = linear.correlation(f, data)
    else if (method == "rank.correlation") 
      x = rank.correlation(f, data)
    else if (method == "information.gain") 
      x = information.gain(f, data)
    else if (method == "gain.ratio") 
      x = gain.ratio(f, data)
    else if (method == "symmetrical.uncertainty") 
      x = symmetrical.uncertainty(f, data)
    else if (method == "chi.squared") 
      x = chi.squared(f, data)
    else if (method == "random.forest.importance") 
      x = random.forest.importance(f, data)
    else if (method == "relief") 
      x = relief(f, data)
    else if (method == "oneR") 
      x = oneR(f, data)
    else
      stop("Unknown filter method: ", method)
    
    val = x[,1]
    names(val) = rownames(x)
    vars = names(which(val > threshold))
    return(list(vars=vars, vals=val))
  } 
)
 

