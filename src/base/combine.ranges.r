#' This function allows it to combine \code{ranges} (i.e. list of named vectors/lists 
#' of possible values for hyperparameters) used in \code{\link{tune}}.
#' 
#' @return A list of \code{ranges}
#' @examples 
#' library(mlbench)
#' 	cv.i <- make.cv.instance(size = nrow(Sonar), iters = 2)
#' 	ct <- make.classif.task("kernlab.svm.classif", data = Sonar, formula = Class~.)
#' 	r1 <- list(kernel = "polydot", C = c(1,2), degree = c(2,3))
#' 	r2 <- list(kernel = "rbfdot", C = c(1,2), sigma = c(4,6))
#' 	tr <- tune(ct, cv.i, ranges = combine.ranges(r1,r2))
#' 
#' @seealso \code{\link{tune}}
#' @title combine.ranges


combine.ranges <- function(...) {
	rs <- list(...)
	names(rs) <- rep("ranges", length(rs))
	return(rs)
}