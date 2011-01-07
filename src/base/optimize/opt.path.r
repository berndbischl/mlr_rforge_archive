
#' Optimazation path. Optimizers log their evaluated points iteratively into this object.
#' Can be subsetted and converted to a list or data.frame.
#'  
#' @exportClass opt.path
#' @title Optimazation path

setClass(
  "opt.path",
  contains = c("object"),
  representation = representation(
    env = "environment"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("opt.path"),
  def = function(.Object) {
    .Object@env$path = list()
    return(.Object)
  }
)


#'  Convert to data.frame
#' @rdname opt.path-class 
#' @export
setMethod(
  f = "as.data.frame",
  signature = signature("opt.path"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    as.data.frame(Reduce(rbind, lapply(as.list(x), as.data.frame)))
  }
)

#'  Convert to list.
#' @rdname opt.path-class 
#' @export
setMethod(
  f = "as.list",
  signature = signature("opt.path"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    x@env$path
  }
)


# todo test!!!
# todo: should subset be integer? where should the help page(s) be?
#' The following 
#' 
#' @export
#' @rdname subset
#' @seealso \code{\link{get.data}} 
#' @title Subset mlr objects.

setMethod(
  f = "subset",
  signature = signature(x="learn.task"),
  def = function (x, subset, select, ...) { 
    if (missing(subset)) 
      r = 1:length(x)
    if (!is.integer(subset)) 
      stop("'subset' must be to integer!")
    if (missing(select)) 
      select = names(env$path[[1]])
    if (!is.character(select)) 
      stop("'select' must be to integer!")
    p = lapply(env$path, function(y) y[select])
    p$env = new.env()
    p$env$path = p[subset]
    p
  }  
)


#this changes the reference! return NULL to remind!
add.path.el = function(op, x) {
  append(op@env$path, x)
  NULL
}


