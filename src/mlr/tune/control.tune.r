#' @include OptControl.r
roxygen()

#' Abstract base class for control objects for tuning. 
#' Cannot be instantiated. 
#' 
#' \describe{
#'   \item{grid.control}{Grid search. All kinds of parameter types can be handled, but you have discretize them yourself by always using \code{\link{makeDiscreteParameter}}}.
#'   \item{optim.control}{\code{\link[stats]{optim}}. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#'   \item{cmaes.control}{CMA Evolution Strategy. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#' }
#' 
#' Subclasses: \code{\linkS4class{grid.control}}, \code{\linkS4class{optim.control}}, \code{\linkS4class{cmaes.control}}
#' 
#' @exportClass tune.control
#' @title Base class for control objects for tuning.

setClass(
		"tune.control",
		contains = c("OptControl"),
		representation = representation(
				start = "list"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("tune.control"),
		def = function(.Object, path, same.resampling.instance, start, ...) {
      if (missing(path))
        return(make.empty(.Object))
      if (!is.null(names(start)))
        stop("'start' has to be a unamed list, but in the correct order!")        
			.Object@start = start 			
			.Object = callNextMethod(.Object=.Object, path=path, same.resampling.instance=same.resampling.instance, ...)
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
        "Same resampling instance: ", x@same.resampling.instance, "\n",
        sep=""
      )
    )
  }
)

