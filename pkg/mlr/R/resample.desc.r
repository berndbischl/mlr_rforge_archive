#' @include resample.instance.r
roxygen()

#'  @export

# todo validation for size
setClass(
		"resample.desc",                                                     
		representation(instance.class="character", name="character", iters="numeric")
)


#----------------- getter ---------------------------------------------------------


setMethod(
		f = "[",
		signature = "resample.desc",
		def = function(x,i,j,...,drop) {
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)

