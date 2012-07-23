#FIXME for which tasks do the filters work?

#' Filter features by using a numerical importance criterion.
#' 
#' Look at package FSelector for details on the filter algorithms. 
#' 
#' @param task [\code{\link[mlr]{SupervisedTask}}]\cr 
#'   The task.  
#' @param method [\code{character(1)}]\cr
#'   Filter method. Available are:
#'   \dQuote{linear.correlation}, \dQuote{rank.correlation}, \dQuote{information.gain}, 
#'   \dQuote{gain.ratio}, \dQuote{symmetrical.uncertainty}, \dQuote{chi.squared}, 
#'   \dQuote{random.forest.importance}, \dQuote{relief}, \dQuote{oneR}
#'   Default is \dQuote{random.forest.importance}.
#' @param threshold [\code{numeric(1)}]\cr
#'   Only features whose importance value exceed this are selected.  
#'   Default is 1.
#' @return A list with the following entries:
#' \describe{
#'   \item{vars [\code{character}]}{Selected features.}
#'   \item{vals [\code{numeric}]}{Importance values for all features used in filter.}
#' }
#' @export
filterFeatures = function(task, method="random.forest.importance", threshold=1) {
  requirePackages("FSelector", "filterFeatures")
  checkArg(task, "SupervisedTask") 
  checkArg(task, "SupervisedTask") 
  checkArg(method, choices=c("linear.correlation", "rank.correlation", "information.gain", 
    "gain.ratio", "symmetrical.uncertainty", "chi.squared", "random.forest.importance", 
    "relief", "oneR"))
  tn = task$task.desc$target
  f = getTaskFormula(task)
  data = getTaskData(task)
  fun = get(method, envir=getNamespace("FSelector"))
  x = fun(f, data)  
  val = x[,1]
  names(val) = rownames(x)
  vars = names(which(val > threshold))
  return(list(vars=vars, vals=val))
} 
 

