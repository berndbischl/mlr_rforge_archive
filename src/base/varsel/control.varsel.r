#' @include opt.control.r
roxygen()

#' Abstract base class for control objects for variable selection. 
#' Cannot be instantiated.
#' 
#' Subclasses: \code{\link{sequential.control}}, \code{\link{randomvarsel.control}}
#' 
#' @exportClass varsel.control
#' @title Base class for control objects for variable selection.

setClass(
		"varsel.control",
		contains = c("opt.control"),
		representation = representation(
				compare = "character",
				max.vars = "integer", 
				maxit = "integer"
		)
)

#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("varsel.control"),
		def = function(.Object, path, same.resampling.instance, maxit, max.vars) {
      if (missing(path))
        return(make.empty(.Object))
			.Object@compare = "diff" 			
			.Object@max.vars = as.integer(max.vars) 			
			.Object@maxit = as.integer(maxit) 		
			.Object = callNextMethod(.Object, path, same.resampling.instance)
			return(.Object)
		}
)

#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("varsel.control"),
  def = function(x) {
    return(
      paste(
        "Control object for varsel of class: ", class(x), "\n",
        "Save path: ", x@path, "\n",
        "Same resampling instance: ", x@same.resampling.instance, "\n",
        "Max. vars: ", x@max.vars, "\n",
        "Max. iter: ", x@maxit,  
        sep=""
      )
    )
  }
)



