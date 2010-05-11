#' @include object.r
roxygen()


setClass(
		"task.desc",
		contains = c("object"),
		representation = representation(
				task.class = "character",
				id = "character",
				label = "character",
				target = "character",
				excluded = "character",
				costs = "matrix",
				positive = "character",
				negative = "character"
	)
)

setMethod(
		f = "[",
		signature = signature("task.desc"),
		def = function(x,i,j,...,drop) {
			callNextMethod()
		}
)



