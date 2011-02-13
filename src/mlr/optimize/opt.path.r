
#' Optimazation path. Optimizers log their evaluated points
#' iteratively into this object.
#' 
#' Can be converted to a list or data.frame.
#' 
#' @exportClass opt.path
#' @title Optimazation path
setClass(
  "OptPath",
  contains = c("object"),
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
      stop("'x.names' and 'y.names' must be unique.")
    if (length(minimize) != length(y.names))
      stop("'y.names' and 'minimize' must be of the same length!")
    .Object@x.names = x.names
    .Object@y.names = y.names
    names(minimize) = y.names
    .Object@minimize = minimize
    .Object@env = new.env()
    .Object@env$path = list()
    .Object@env$dob  = integer()
    .Object@env$eol  = integer()
    return(.Object)
  }
)

#' @export 
makeOptPath = function(x.names, y.names, minimize) {
  new("OptPath", x.names, y.names, minimize)
}

#' Convert to data.frame
#' @rdname OptPath-class 
#' @export
setMethod(
  f = "as.data.frame",
  signature = signature("OptPath"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    df <- do.call(rbind, lapply(x@env$path, function(e) cbind(as.data.frame(e$x),as.data.frame(t(e$y)))))
    colnames(df)[(ncol(df)-length(x@y.names)+1):ncol(df)] = x@y.names
    df[[".dob"]] <- x@env$dob
    df[[".eol"]] <- x@env$eol
    df
  }
)

#' Convert an optimization path to a list.
#' @rdname OptPath-class
setMethod(
  f = "as.list",
  signature = signature("OptPath"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    l <- x@env$path
  }
)

##' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("OptPath"),
  def = function(x) {
    return(paste("Opt. path of length: ", length(as.list(x))))
  }
)

#' Add a new element to the optimiztion path.
#' 
#' @param op [\code{\linkS4class{OptPath}}] \cr 
#'   Optimization path.  
#' @param x [list]\cr 
#'   List of parameter settings for a point in input space.   
#' @param y [numeric] \cr 
#'   Vector of fitness values. 
#' @param dob [integer(1)] \cr 
#'   Date of birth of the new parameters. Default is length of path + 1.  
#' @param eol [integer(1)] \cr 
#'   End of life of point. Defaults to unknown (NA). 
#' @return NULL. This function is called for its side effects, namely
#'   adding \code{x} to the optimization path.
#' @export 
add.path.el = function(op, x, y, dob, eol=NA) {
  stopifnot(inherits(op, "OptPath"),          
            is.na(eol) || eol >= dob)
  if (missing(dob))        
    dob=length(op@env$path)+1
  op@env$path = append(op@env$path, list(list(x=x, y=y)))
  op@env$dob = append(op@env$dob, dob)
  op@env$eol = append(op@env$eol, eol)
  NULL
}

#' Convert a parameter list to its position in the optimiztion path.
#'
#' @param op Optimization path.
#' @param x List of parameter settings.
#' @param cand Expression limiting the path elements searched.
#' @return Index of \code{x} in optimization path or if \code{x} is
#'  not present \code{NA}.
param.to.position <- function(op, x, cand) {
  if (!missing(cand)) {
    r <- eval(eval(substitute(substitute(cand, op@env))), parent.frame())
    idx <- which(r)
    tmp <- Position(function(zz) identical(x, zz), op@env$path[idx])
    if (!is.na(tmp))
      idx[tmp]
    else
      tmp
  } else {
    Position(function(e) identical(x, e$x), op@env$path)
  }
}

#' Set the end of life of a parameter vector.
#'
#' @param op [\code{\linkS4class{OptPath}}] \cr 
#'   Optimization path.  
#' @param x [integer(1) | list]\cr 
#'   List of parameter settings for a point in input space or an integer index for a path element.   
#' @param eol [integer(1)] \cr 
#'   End of life of point. 
#' @return NULL. This function is called for its side effects, namely
#'   adding \code{x} to the optimization path.
#' @export 
#' @return NULL, this function is called for its side effect, namely
#'   modifing the optimization path.
setEoL = function(op, x, eol) {
  if (is.numeric(x) && length(x) == 1 && x == as.integer(x))
    x = as.integer(x)
  if (!is.integer(x))
    x = param.to.position(op, x)
  if (is.na(x))
    stop("No element found matching the given parameter settings. Cannot set EoL!")
  op@env$eol[x] = as.integer(eol)
  NULL
} 

#' Get the date of birth or end of life a list parameter settings.
#'
#' @param op Optimization path.
#' @param z List of parameter settings.
#' @return The date of birth or end of life of \code{z}.
get.dob = function(op, z) {
  stopifnot(inherits(op, "OptPath"))
  if (!is.integer(z)) {
    z <- param.to.position(op, z)
    if (is.na(tmp))
      stop("No element found matching the given parameter settings. Cannot get DoB!")
  }
  op@env$dob[z]
}

#' @rdname get.dob
get.eol = function(op, z) {
  stopifnot(inherits(op, "OptPath"))
  if (!is.integer(z)) {
    z <- param.to.position(op, z)
    if (is.na(z))
      stop("No element found matching the given parameter settings. Cannot get EoL!")
  }
  op@env$eol[z]
}



#' Subset optimiztion path.
#'
#' @param op [\code{\linkS4class{OptPath}}]\cr
#'   Optimization path.
#' @param dob [integer]
#'   Possible dates of birth for subset. Defaults to all. 
#' @param eol [integer]
#'   Possible end of life for subset. Defaults to all. 
#' @return Subsetted \code{\linkS4class{OptPath}}. 
#' @export
setMethod(
  f = "subset",
  signature = signature(x="OptPath"),
  def = function(x, dob=x@env$dob, eol=x@env$eol) {
    p = x@env$path[x@env$dob %in% dob & x@env$eol %in% eol]
    op = new("OptPath", x@x.names, x@y.names, x@minimize)
    op@env$path = p
    return(op)
  }
)

#' Get best element from optimiztion path.
#'
#' @param op [\code{\linkS4class{OptPath}}]\cr
#'   Optimization path.
#' @param y.name [character(1)] 
#'   Name of target value to decide which element is best.  
#' @param dob [integer]
#'   Possible dates of birth to select best element from. Defaults to all. 
#' @param eol [integer]
#'   Possible end of life to select best element from. Defaults to all. 
#' @return List with elements 'x' [list] and 'y' [named numeric]. 
#' @export
getBestElement = function(op, y.name=op@y.names[1], dob=op@env$dob, eol=op@env$eol) {
  op = subset(op, dob, eol)
  y = sapply(op@env$path, function(e) e$y[y.name])
  if (op@minimize[y.name])
    i = which.min(y)
  else 
    i = which.max(y)
  if (all(is.na(y))) {
    warning("All elements had NA fitness, selecting 1 randomly!")
    i = sample(1:length(y), 1)
  }
  return(op@env$path[[i]])
}

