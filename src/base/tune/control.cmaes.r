#' @export 

cmaes.control <- function(start, ...) {
	x = list(...)
	y = list(start=unlist(start))
	y = c(y, x)
	class(y) = "cmaes.control"
	return(y)
}
