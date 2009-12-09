
add.data()


add.dim <- function(bench, dim, name) {
	x = 1:4
	names(x) = c("samples", "algorithms", "performances", "data sets")
	if (is.numeric(dim))
		dim = x[dim]
	y = array(NA, dim=dim(bench)[-dim])
	bench = abind(bench, y, along=dim)
}
