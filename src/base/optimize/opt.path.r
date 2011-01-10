
#' Optimazation path. Optimizers log their evaluated points iteratively into this object.
#' Can be subsetted and converted to a list or data.frame.
#'  
#' @exportClass opt.path
#' @title Optimazation path

setClass(
  "opt.path",
  contains = c("object"),
  representation = representation(
    x.names = "character",
    y.names = "character",
    env = "environment"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("opt.path"),
  def = function(.Object, x.names, y.names) {
    .Object@x.names = x.names
    .Object@y.names = y.names
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


#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("opt.path"),
  def = function(x) {
    return(paste("Opt. path of length: ", length(as.list(x))))
  }
)


#this changes the reference! return NULL to remind!
add.path.el = function(op, x, dob=length(as.list(op))) {
  x$dob = dob
  x$tod = as.integer(NA)
  op@env$path = append(op@env$path, list(x))
  NULL
}


set.tod = function(op, x) {
  
} 

library(RUnit)

  op = new("opt.path", x.names=c("x1", "x2"), y.names=c("y1", "y2"))
  add.path.el(op, list(x1=1, x2="a", y1=1, y2=3))
  add.path.el(op, list(x1=2, x2="a", y1=1, y2=3))
  
  x = as.list(op)
  checkTrue(is.list(x))
  checkEquals(length(x), 2)
  checkTrue(is.list(x[[1]]))
  checkTrue(is.list(x[[2]]))
  checkEquals(length(x[[1]]), 4)
  checkEquals(length(x[[2]]), 4)
  
  x = as.data.frame(op)
  checkTrue(is.data.frame(x))
  checkEquals(nrow(x), 2)
  checkEquals(ncol(x), 4)


