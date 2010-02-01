# if as.character is used this is exported automatically and clutters up docs.... :(
setGeneric(
		name = "to.string",
		def = function(x) {
			standardGeneric("to.string")
		}
)
