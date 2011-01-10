
#' Bounds for a bunch of parameters. Are initially created, then mainly queried for information but not mutated. 
#'  
#' @exportClass opt.path
#' @title Optimazation path

setClass(
  "bounds",
  contains = c("object"),
  representation = representation(
    vars = "list"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("bounds"),
  def = function(.Object, vars) {
    for (i in seq(length=length(vars))) {
      if (!is(vars[[i]], "par.desc"))
        stop("All bounds vars must be of type 'par.desc'!")
    }
    .Object@vars = vars
    return(.Object)
  }
)

#'  Convert to list.
#' @rdname bounds-class 
#' @export
setMethod(
  f = "as.list",
  signature = signature("bounds"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    z = x@vars
    names(z) = sapply(z, function(y) y["par.name"])
    z
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

#' Getter.
#' @rdname bounds-class

setMethod(
  f = "[",
  signature = signature("bounds"),
  def = function(x,i,j,...,drop) {
    if (i == "names")  {
      return(sapply(x@vars, function(y) y["par.name"]))
    }     
    if (i == "nums")  {
      v = Filter(function(y) is(y, "par.desc.double") && y["data.type"] == "numeric", x@vars)
      return(sapply(v, function(y) y["par.name"]))
    }     
    if (i == "ints")  {
      v = Filter(function(y) is(y, "par.desc.double") && y["data.type"] == "integer", x@vars)
      return(sapply(v, function(y) y["par.name"]))
    }     
    if (i == "discs")  {
      v = Filter(function(y) is(y, "par.desc.disc"), x@vars)
      return(sapply(v, function(y) y["par.name"]))
    }     
    if (i == "lower")  {
      v = Filter(function(y) is(y, "par.desc.double"), x@vars)
      z = sapply(v, function(y) y["lower"])
      names(z) = sapply(v, function(y) y@par.name)
      return(z)
    }     
    if (i == "upper")  {
      v = Filter(function(y) is(y, "par.desc.double"), x@vars)
      z = sapply(v, function(y) y["upper"])
      names(z) = sapply(v, function(y) y@par.name)
      return(z)
    }     
    if (i == "vals")  {
      v = Filter(function(y) is(y, "par.desc.disc"), x@vars)
      z = lapply(v, function(y) y["vals"])
      names(z) = sapply(v, function(y) y@par.name)
      return(z)
    }     
    callNextMethod()
  }
)


#' Construct a bounds object.
#' 
#' @param vars [list of \code{\linkS4class{learn.task}}] \cr
#' 			Type of the learning algorithm, either "classif" or "regr" or task to solve
#' @param doubles [boolean] \cr
#' 			Supports real-valued inputs? Pass only when x is a string.
#' @param factors [boolean] \cr
#' 			Supports factor inputs? Pass only when x is a string.
#' @param characters [boolean] \cr
#' 			Supports character inputs? Pass only when x is a string.
#' @param missings [boolean] \cr
#' 			Supports missing values? Pass only when x is a string.
#' @param multiclass [boolean] \cr
#' 			Supports multiclass problems? Pass only when x is a string.
#' @param weights [boolean] \cr
#' 			Supports case weights? Pass only when x is a string.
#' @param probs [boolean] \cr
#' 			Can predict probabilities?
#' @param decision [boolean] \cr
#' 			Supports decision values?
#' @param costs [boolean] \cr
#' 			Supports non-standard misclassification costs?
#' 
#' @rdname get.learners
#' @export 
#' 
#' @title Find matching learning algorithms.


make.bounds = function(vars) {
  new("bounds", vars=vars)
}

source("D:\\sync\\projekte\\mlr\\src\\base\\helpers.r")
source("D:\\sync\\projekte\\mlr\\src\\base\\par.desc.r")

b = make.bounds(list(
    new("par.desc.double", par.name="x1", lower=2, upper=7),
    new("par.desc.double", par.name="x2", lower=3L),
    discrete.learner.parameter(par.name="x3", vals=c("a", "b"))
))


setMethod(
  f = "is.feasible",
  signature = signature(x="list", bounds="bounds"),
  def = function(x, bounds) {
    ns = names(x)
    stopifnot(all(ns %in% bounds["names"]))
    for (i in seq(length=length(x))) {
      if (!is.feasible(x[[i]], bounds@vars[[ns[i]]]))
        return(FALSE)
    }  
    TRUE
  }
)




