setClass(
		"task.desc",
		contains = c("object"),
		representation = representation(
				task.class = "character",
				name = "character",
				target = "character",
				excluded = "character",
				weights = "numeric",
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




#if (i == "negative") {
#	if (x["is.binary"])
#		return(setdiff(x["class.levels"], x["positive"]))
#	return(NA)
#}
