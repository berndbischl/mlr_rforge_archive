
#' @exportClass OptPath
#' @title Optimazation path
setClass(
  "OptPath",
  representation = representation(
    x.names = "character",
    y.names = "character",
    minimize = "logical",
    env = "environment"
  )
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("OptPath"),
  def = function(.Object, x.names, y.names, minimize) {
    if (length(intersect(x.names, y.names)) > 0)
      stop("'x.names' and 'y.names' must not contain common elements!")
    if (length(minimize) != length(y.names))
      stop("'y.names' and 'minimize' must be of the same length!")
    if (is.character(names(minimize)) && !setequal(names(minimize), y.names))
      stop("Given names for 'minimize' must be the same as 'y.names'!")
    if (is.null(names(minimize)))
      names(minimize) = y.names
    
    if (any(c("dob", "eol") %in% (union(x.names, y.names))))
      stop("'dob' and 'eol' are not allowed in 'x.names' or 'y.names'!")
    .Object@x.names = x.names
    .Object@y.names = y.names
    .Object@minimize = minimize
    .Object@env = new.env()
    .Object@env$path = data.frame()
    .Object@env$dob  = integer()
    .Object@env$eol  = integer()
    return(.Object)
  }
)



#' @rdname addPathElement
setMethod(
  f = "addPathElement",
  
  signature = signature(op="OptPathList", x="any", x.trafo="any", y="any", dob="integer", eol="integer"), 
  
  def = function(id, x, x.trafo, y, dob, eol) {
    dob=length(op@env$path)+1
    names(x.trafo) = op@x.names
    names(y) = op@y.names
    op@env$path = append(op@env$path, list(list(x=x.trafo, y=y)))
    op@env$dob = append(op@env$dob, dob)
    op@env$eol = append(op@env$eol, eol)
  }
)
