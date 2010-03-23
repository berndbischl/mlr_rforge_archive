nm.control <- function(start, ...) {
	xs = list(...)
	ys = list(start=unlist(start))
	c(ys, xs)
}
