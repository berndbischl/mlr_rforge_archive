#' @include opt.control.r
roxygen()

#' Abstract base class for control objects for tuning. 
#' Cannot be instatiated. 
#' 
#' @exportClass tune.control
#' @seealso \code{\linkS4class{grid.control}}, \code{\linkS4class{optim.control}}, \code{\linkS4class{cmaes.control}} 
#' @title Base class for control objects for tuning.

setClass(
		"tune.control",
		contains = c("opt.control"),
		representation = representation(
				start = "list",
				par.descs = "list",
				scale = "function"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("tune.control"),
		def = function(.Object, path, start, par.descs, scale, ...) {
      if (missing(path))
        return(make.empty(.Object))
			.Object@start = start 			
			.Object@par.descs = par.descs 			
			.Object@scale = scale 		
			.Object = callNextMethod(.Object=.Object, path=path, ...)
			return(.Object)
		}
)


#' @rdname tune.control-class

setMethod(
		f = "[",
		signature = signature("tune.control"),
		def = function(x,i,j,...,drop) {
			pds = x@par.descs
			if (i == "par.names") {
				return(sapply(pds, function(y) y@par.name))
			}
			if (i == "lower") {
				y = sapply(pds, function(y) ifelse(is(y, "par.desc.num"), y@lower, NA))
				names(y) = x["par.names"]
        return(Filter(function(w) !is.na(w), y))
      }
			if (i == "upper") {
				y = sapply(pds, function(y) ifelse(is(y, "par.desc.num"), y@upper, NA))
				names(y) = x["par.names"] 
        return(Filter(function(w) !is.na(w), y))
      }		
      if (i == "vals") {
        y = lapply(pds, function(y) if(is(y, "par.desc.disc")) y@vals else NA)
        names(y) = x["par.names"] 
        return(Filter(function(w) !identical(w, NA), y))
      }   
      callNextMethod(x,i,j,...,drop=drop)
		}
)


#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("tune.control"),
  def = function(x) {
    ns = paste(x["par.names"], collapse=",")    
    lo = paste(x["lower"], collapse=",")    
    up = paste(x["upper"], collapse=",")
    ss = unlist(x["start"])
    ss =  paste(x["start"], collapse=",")    
    vals = x["vals"]
    if (length(vals) > 0) {
      vals = sapply(vals, function(y) paste("has ", length(y), " vals", sep=""))
      vals = paste(names(vals), vals)
      vals = paste("Discrete vals:", paste(vals, collapse=","))
    } else {
      vals = ""
    }
    
    return(
      paste(
        "Control object for tuning of class: ", class(x), "\n",
        "Optimized parameters: ", ns, "\n",
        "Save path: ", x@path, "\n",
        "Scaling function used: ", !identical(x@scale, identity), "\n",
        "Lower bounds: ", lo, "\n",
        "Upper bounds: ", up, "\n",
        "Start values: ", ss, "\n",
        vals,       
        sep=""
      )
    )
  }
)

