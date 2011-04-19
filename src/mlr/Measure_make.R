#' Construct your own performance measure.
#' 
#' @param id [\code{character(1)}] \cr
#'   Name of measure. 
#' @param minimize [\code{logical(1)}] \cr
#'   Should the measure be minimized? Default is TRUE. 
#' @param classif [\code{logical(1)}] \cr
#'   Is the measure applicable for classification? Default is FALSE.   
#' @param regr [\code{logical(1)}] \cr
#'   Is the measure applicable for regression? Default is FALSE.   
#' @param only.binary [\code{logical(1)}] \cr
#'   Is the measure only applicable to binary classification? 
#'   Only reasonable if \code{classif} is \code{TRUE}. 
#'   Default is \code{FALSE}. 
#' @param allowed.pred.types [\code{character}]
#'   Which prediction types are allowed for this measure? 
#'   Subset of \dQuote{response},\dQuote{prob} and \dQuote{decision}.
#'   Default is \code{character(0)}.   
#' @param fun [\code{function(task, model, pred, extra.args)}] \cr
#'   Calculates performance value. 
#' @param extra.args [\code{list}] \cr
#'   List of extra arguments which will always be passed to fun.   	  
#' 
#' @return \code{\linkS4class{Measure}} 
#' 
#' @exportMethod makeMeasure
#' @rdname makeMeasure
#' @seealso \code{\link{measures}}
#' @title Construct your own performance measure.
#' @examples
#'   f = function(task, model, pred, extra.pars) sum((pred@@df$response - pred@@df$truth)^2) 
#'   makeMeasure(id="my.sse", minimize=TRUE, regr=TRUE, allowed.pred.types=c"response", fun=f)

setGeneric(
  name = "makeMeasure",
  def = function(id, minimize, classif, regr, only.binary, allowed.pred.types, fun, extra.args) {
    if (missing(classif))
      classif = FALSE
    if (missing(regr))
      regr = FALSE
    if (missing(only.binary))
      only.binary = FALSE
    if (missing(allowed.pred.types))
      allowed.pred.types = character(0)
    if (missing(extra.args))
      extra.args = list()
    standardGeneric("makeMeasure")
  }
)


#' @rdname makeMeasure
setMethod(
  f = "makeMeasure",
  signature = signature(id="character", minimize="logical", classif="logical", regr="logical", only.binary="logical", 
    allowed.pred.types="character", fun="function", extra.args="list"),
  def = function(id, minimize, classif, regr, only.binary, allowed.pred.types, fun, extra.args) {
    fun1 = fun
    formals(fun1) = list()
    v = codetools:::findGlobals(fun1, merge=FALSE)$variables
    if (only.binary && !classif)
      stop("only.binary can only be set to TRUE, if 'classif' is set to TRUE!")
    new("Measure", id=id, fun=fun, extra.args=extra.args, minimize=minimize, classif=classif, regr=regr, only.binary=only.binary,
      allowed.pred.types=allowed.pred.types, req.pred="pred" %in% v, req.model="model" %in% v, req.task="task" %in% v,
      aggr = list(test.mean, test.sd)
    )
  }
)

default.measures = function(x) {
  if (x@desc@type == "classif")
    return(list(mmce))
  else 
    return(list(mse))
}

