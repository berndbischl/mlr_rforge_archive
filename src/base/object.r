
#' @exportClass object

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


#' General method to convert object to strings.
#' @rdname to.string 

setGeneric(
		name = "to.string",
		def = function(x) {
			standardGeneric("to.string")
		}
)

#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("object"),
		def = function(x) {
			return(class(x))
		}
)

#' Prints the object by calling as.character.
setMethod(
		f = "print",
		signature = signature("object"),
		def = function(x, ...) {
			cat(to.string(x), "\n")
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("object"),
		def = function(object) {
			cat(to.string(object), "\n")
		}
)

check.getter = function(x,i,j,...,drop) {
	if (!missing(j))
		stop("You are not allowed to use argument j in a mlr getter!")
}


