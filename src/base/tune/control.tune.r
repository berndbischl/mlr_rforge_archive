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
				start = "list"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("tune.control"),
		def = function(.Object, path, start, ...) {
      if (missing(path))
        return(make.empty(.Object))
			.Object@start = start 			
			.Object = callNextMethod(.Object=.Object, path=path, ...)
			return(.Object)
		}
)


#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("tune.control"),
  def = function(x) {
    
    return(
      paste(
        "Control object for tuning of class: ", class(x), "\n",
        "Save path: ", x@path, "\n",
        sep=""
      )
    )
  }
)

