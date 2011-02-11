#' Construct your own performance measure.
#' 
#' @param id [string] \cr
#'   Name of measure. 
#' @param minimize [boolean] \cr
#'   Should the measure be minimized? Default is TRUE. 
#' @param req.task.type [string] \cr
#'   Should the measure be minimized? Default is TRUE. Otherwise you are effectively specifying a benefits matrix.
#' @param req.binary [boolean] \cr
#'   Is the measure only applicable to binary classification? Only reasonable if \code{req.task.type} is "classif". Default is FALSE. 
#' @param fun [function] \cr
#'   Calculates performance value. Must have signature (task, model, pred, extra.pars). 
#' @param extra.pars [list] \cr
#'   List of extra arguments which will always be passed to fun.   	  
#' 
#' @return \code{\linkS4class{measure}} 
#' 
#' @exportMethod makeMeasure
#' @rdname makeMeasure
#' @seealso \code{\link{measures}}, \code{\link{makeMeasure}}
#' @title Construct your own performance measure.

setGeneric(
  name = "makeMeasure",
  def = function(id, minimize, req.task.type, req.binary, req.pred.type, fun, extra.pars) {
    if (missing(req.task.type))
      req.task.type = c("classif", "regr")
    if (missing(req.binary))
      req.binary = FALSE
    if (missing(req.pred.type))
      req.pred.type = c("response", "prob", "decsison")
    if (missing(extra.pars))
      extra.pars = list()
    standardGeneric("makeMeasure")
  }
)


setMethod(
  f = "makeMeasure",
  signature = signature(id="character", minimize="logical", req.task.type="character", req.binary="logical", 
    req.pred.type="character", fun="function", extra.pars="list"),
  def = function(id, minimize, req.task.type, req.binary, req.pred.type, fun, extra.pars) {
    fun1 = fun
    formals(fun1) = list()
    v = codetools:::findGlobals(fun1, merge=FALSE)$variables
    if (req.binary && !identical(req.task.type, "classif"))
      stop("req.binary can only be set to TRUE, if req.task.type is set to 'classif'!")
    new("Measure", id=id, fun=fun, extra.pars=extra.pars, minimize=minimize, req.task.type=req.task.type, req.binary=req.binary,
      req.pred.type=req.pred.type, req.pred="pred" %in% v, req.model="model" %in% v, req.task="task" %in% v,
      aggr = list(test.mean, test.sd)
    )
  }
)

default.measures = function(x) {
  if (x["is.classif"])
    return(list(mmce))
  else 
    return(list(mse))
}

