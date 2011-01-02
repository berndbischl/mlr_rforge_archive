#' @include control.tune.r
roxygen()

#' @exportClass grid.control
#' @rdname grid.control 

setClass(
		"grid.control",
		contains = c("tune.control")
)

#' Control structure for grid search tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
#' @param scale [\code{\link{function}}] \cr 
#'        A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a vector and return a scaled one. Default is identity function.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod grid.control
#' @rdname grid.control 
#' @title Control for grid search tuning. 


setGeneric(
		name = "grid.control",
		def = function(path, ranges, scale) {
			if (missing(path))
				path=TRUE
			if (missing(ranges))
				ranges=list()
			if (missing(scale))
				scale=identity
			standardGeneric("grid.control")
		}
)


#' @rdname grid.control 

setMethod(
		f = "grid.control",
		signature = signature(path="logical", ranges="list", scale="function"),
		def = function(path, ranges, scale) {
			pds = list()
			for (i in 1:length(ranges)) {
				pd = new("par.desc.disc", par.name=names(ranges)[i], vals=as.list(ranges[[i]]))
				pds[[i]] = pd 
			}
			new("grid.control", path=path, start=list(), par.descs=pds, scale=scale)
		}
)
