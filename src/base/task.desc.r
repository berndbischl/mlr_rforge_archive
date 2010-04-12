#' @include object.r
roxygen()


setClass(
		"task.desc",
		contains = c("object"),
		representation = representation(
				task.class = "character",
				name = "character",
				target = "character",
				excluded = "character",
				costs = "matrix",
				positive = "character",
				negative = "character"
	)
)

#' @rdname undocumented

setMethod(
		f = "[",
		signature = signature("task.desc"),
		def = function(x,i,j,...,drop) {
			callNextMethod()
		}
)




#if (i == "negative") {
#	if (x["is.binary"])
#		return(setdiff(x["class.levels"], x["positive"]))
#	return(NA)
#}
