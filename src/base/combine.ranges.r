combine.ranges <- function(...) {
	rs <- list(...)
	names(rs) <- rep("ranges", length(rs))
	return(rs)
}