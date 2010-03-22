

setClass(
		"object"
)


setMethod(
		f = "[",
		signature = signature("object"),
		def = function(x,i,j,...,drop) {
			if (i %in% slotNames(x))
				return(eval(substitute("@"(x, slot), list(slot=i))))
			return(NULL)
		}
)
