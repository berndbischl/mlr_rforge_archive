#' @include opt.control.r
roxygen()

#' Abstract base class for control objects for variable selection. 
#' Cannot be instatiated. 
#' 
#' @exportClass varsel.control
#' @seealso \code{\link{sequential.control}}, \code{\link{randomvarsel.control}} 
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
		def = function(.Object, path, maxit, max.vars) {
      if (missing(path))
        return(make.empty(.Object))
			.Object@compare = "diff" 			
			.Object@max.vars = as.integer(max.vars) 			
			.Object@maxit = as.integer(maxit) 		
			.Object = callNextMethod(.Object=.Object, path=path)
			return(.Object)
		}
)

