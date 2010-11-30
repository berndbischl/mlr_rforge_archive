
#' Create a performance measure.
#' 
#' @param id [string] \cr
#'   Name of measure. Default is "costs".
#' @param minimize [boolean] \cr
#'   Should the measure be minimized? Default is TRUE. Otherwise you are effectively specifying a benefits matrix.
#' @param req.task.type [boolean] \cr
#'   Should the measure be minimized? Default is TRUE. Otherwise you are effectively specifying a benefits matrix.
#' @param fun [function] \cr
#'   Should the measure be minimized? Default is TRUE. Otherwise you are effectively specifying a benefits matrix.
#' @param extra.pars [list] \cr
#'   Matrix of misclassification costs. Rows and columns have to be named with class labels, order does not matter. 
#'   Rows indicate predicted and columns the true classes.
#' 
#' @return \code{\linkS4class{measure}} 
#' 
#' @exportMethod make.cost.measure
#' @rdname make.cost.measure
#' @seealso \code{\link{measures}}, \code{\link{make.measure}}
#' @title Construct performance measure.

setGeneric(
  name = "make.measure",
  def = function(id, minimize, req.task.type, req.pred.type, fun, extra.pars) {
    if (missing(req.task.type))
      req.task.type = c("classif", "regr")
    if (missing(req.pred.type))
      req.pred.type = c("response", "prob", "decsison")
    if (missing(extra.pars))
      extra.pars = list()
    standardGeneric("make.measure")
  }
)


setMethod(
  f = "make.measure",
  signature = signature(id="character", minimize="logical", req.task.type="character", req.pred.type="character", fun="function", extra.pars="list"),
  def = function(id, minimize, req.task.type, req.pred.type, fun, extra.pars) {
    fun1 = fun
    formals(fun1) = list()
    v = codetools:::findGlobals(fun1, merge=FALSE)$variables
    new("measure", id=id, fun=fun, extra.pars=extra.pars, minimize=minimize, req.task.type=req.task.type, req.pred.type=req.pred.type, 
      req.pred.test="pred.test" %in% v, req.pred.train="pred.train" %in% v, req.model="model" %in% v, req.task="task" %in% v)
  }
)



make.measures = function(xs) {
  if (length(xs)==0)
    return(list())
  # single function to list
  if (is(xs, "measure")) {
    return(list(xs))
  }
  return(xs)
  return(ys)	
}

default.measures = function(x) {
  if (x["is.classif"])
    return(list(mmce))
  else 
    return(list(mse))
}

