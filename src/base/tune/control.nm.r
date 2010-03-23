#' @export 

nm.control <- function(start, ...) {
	x = list(...)
	y = list(start=unlist(start))
	y = c(y, x)
	class(y) = "nm.control"
	return(y)
}
