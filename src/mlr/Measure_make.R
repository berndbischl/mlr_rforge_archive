#' Construct your own performance measure.
#'
#' @title Construct your own performance measure.
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized?
#'   Default is TRUE.
#' @param classif [\code{logical(1)}]\cr
#'   Is the measure applicable for classification?
#'   Default is FALSE.
#' @param regr [\code{logical(1)}]\cr
#'   Is the measure applicable for regression?
#'   Default is FALSE.
#' @param only.binary [\code{logical(1)}]\cr
#'   Is the measure only applicable to binary classification?
#'   Only reasonable if \code{classif} is \code{TRUE}.
#'   Default is \code{FALSE}.
#' @param allowed.pred.types [\code{character}]\cr
#'   Which prediction types are allowed for this measure?
#'   Subset of \dQuote{response},\dQuote{prob}.
#'   Default is \code{character(0)}.
#' @param fun [\code{function(task, model, pred, extra.args)}]\cr
#'   Calculates performance value.
#' @param extra.args [\code{list}]\cr
#'   List of extra arguments which will always be passed to fun.      
#'   Default is empty list.
#' @return \code{\linkS4class{Measure}}
#' @export
#' @seealso \code{\link{measures}}
#' @examples
#'   f <- function(task, model, pred, extra.args) sum((pred@@df$response - pred@@df$truth)^2)
#'   makeMeasure(id="my.sse", minimize=TRUE, regr=TRUE, allowed.pred.types="response", fun=f)
makeMeasure = function(id, minimize, classif=FALSE, regr=FALSE,
  only.binary=FALSE, allowed.pred.types=character(0), fun, extra.args=list()) {

  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(minimize, "logical", len=1, na.ok=FALSE)
  checkArg(classif, "logical", len=1, na.ok=FALSE)
  checkArg(regr, "logical", len=1, na.ok=FALSE)
  checkArg(only.binary, "logical", len=1, na.ok=FALSE)
  checkArg(allowed.pred.types, subset=c("response", "prob", "se"))
  checkArg(fun, "function")
  checkArg(extra.args, "list")

  fun1 = fun
  formals(fun1) = list()
  v = codetools:::findGlobals(fun1, merge=FALSE)$variables
  if (only.binary && !classif)
    stop("only.binary can only be set to TRUE, if 'classif' is set to TRUE!")
  m = new("Measure", id=id, fun=fun, extra.args=extra.args, minimize=minimize, classif=classif, regr=regr, only.binary=only.binary,
    allowed.pred.types=allowed.pred.types, req.pred="pred" %in% v, req.model="model" %in% v, req.task="task" %in% v
  )
  setAggregation(m, test.mean)
}

default.measures = function(x) {
  if (is(x, "LearnTask")) {
    if (x@desc@type == "classif")
      return(list(mmce))
    else if (x@desc@type == "regr")
      return(list(mse))
    else
      stop("Should not happen!")
  }
  if (is(x, "Learner")) {
    if (getProperty(x, "type") == "classif")
      return(list(mmce))
    else if (getProperty(x, "type") == "regr")
      return(list(mse))
    else
      stop("Should not happen!")
  }
} 