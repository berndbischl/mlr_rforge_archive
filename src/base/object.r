

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

# if as.character is used this is exported automatically and clutters up docs.... :(
setGeneric(
		name = "to.string",
		def = function(x) {
			standardGeneric("to.string")
		}
)

setMethod(
		f = "to.string",
		signature = signature("object"),
		def = function(x) {
			return(class(x))
		}
)

