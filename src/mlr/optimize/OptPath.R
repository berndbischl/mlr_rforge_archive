
#' Optimazation path. Optimizers log their evaluated points
#' iteratively into this object.
#' 
#' Can be converted to a list or data.frame.
#' 
#' @exportClass OptPath
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
    if (any(c("dob", "eol") %in% (union(x.names, y.names))))
      stop("'dob' and 'eol' are not allowed in 'x.names' or 'y.names'!")
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
    df = flattenX(x)
    ys = as.data.frame(Reduce(rbind, lapply(x@env$path, function(el) el$y)))
    df = cbind(df, ys)
    xns = x@x.names
    el1x = x@env$path[[1]]$x
    nn = sapply(el1x, length)
    xns = Reduce(c, Map(function(x, n) if(n==1) x else paste(x, 1:n, sep=""), xns, nn))
    colnames(df) = c(xns, x@y.names)
    df[["dob"]] <- x@env$dob
    df[["eol"]] <- x@env$eol
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
#'   List of parameter settings for a point in input space. Must be in same order as \code{x.names}.  
#' @param y [numeric] \cr 
#'   Vector of fitness values.  Must be in same order as \code{y.names}.
#' @param dob [integer(1)] \cr 
#'   Date of birth of the new parameters. Default is length of path + 1.  
#' @param eol [integer(1)] \cr 
#'   End of life of point. Defaults to unknown (NA). 
#' @return NULL. This function is called for its side effects, namely
#'   adding \code{x} to the optimization path.
#' @export 
addPathElement = function(op, x, y, dob, eol=NA) {
  stopifnot(inherits(op, "OptPath"),          
            is.na(eol) || eol >= dob)
  if (missing(dob))        
    dob=length(op@env$path)+1
  names(x) = op@x.names
  names(y) = op@y.names
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
    # we do not check names / attribs
    tmp <- Position(function(zz) all.equal(x, zz, check.attributes=FALSE), op@env$path[idx])
    if (!is.na(tmp))
      idx[tmp]
    else
      tmp
  } else {
    Position(function(e) isTRUE(all.equal(x, e$x, check.attributes=FALSE)), op@env$path)
  }
}

#' Set the end of life of a parameter vector.
#'
#' @param op [\code{\linkS4class{OptPath}}] \cr 
#'   Optimization path.  
#' @param x [list]\cr 
#'   List of parameter settings for a point in input space.   
#' @param eol [integer(1)] \cr 
#'   End of life of point. 
#' @export 
#' @return NULL, this function is called for its side effect, namely
#'   modifing the optimization path.
setEoL = function(op, x, eol) {
  x = param.to.position(op, x)
  if (is.na(x))
    stop("No element found matching the given parameter settings. Cannot set EoL!")
  op@env$eol[x] = as.integer(eol)
  NULL
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


flattenX = function(op) {
  Reduce(rbind, lapply(op@env$path, function(el) {  
    Reduce(cbind, lapply(el$x, function(a) {
      as.data.frame(t(a))    
    }))
  }))
}



