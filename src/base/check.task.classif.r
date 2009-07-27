#' @include task.classif.r
roxygen()

setGeneric(
		name = "check.task.classif",
		def = function(lt) {
			standardGeneric("check.task.classif")
		}
)

setMethod(
		f = "check.task.classif",
		signature = c(lt="classif.task"),
		def = function(lt) {
			wl <- lt@wrapped.learner
			ld <- wl@learner.props
			dd <- lt@data.desc
			data <- lt@data
			msg <- ""
			if (dd@integers > 0) {
				logger.warn("Your data set contains integer variables. These will be converted to numerics! You should better look at those carefully and convert them yourself to either numerics or factors!")
				data <- as.data.frame(
						lapply(data, function (x) if(is.integer(x)) as.numeric(x) else x),
						stringsAsFactors=FALSE	  
				)
			}
			if (dd@class.nr > 2 && !ld@supports.multiclass) {
				msg <- paste("Data set is a multiclass-problem, but", wl@learner.name, "does not support that!")
			}
			if (dd@has.missing && !ld@supports.missing) {
				msg <- paste("Data set has missing values, but", wl@learner.name, "does not support that!")
			}
			if (dd@numerics > 0 && !ld@supports.numerics) {
				msg <- paste("Data set has numeric inputs, but", wl@learner.name, "does not support that!")
			}
			if (dd@factors > 0 && !ld@supports.factors) {
				msg <- paste("Data set has factor inputs, but", wl@learner.name, "does not support that!")
			}
			if (dd@characters > 0 && !ld@supports.characters) {
				msg <- paste("Data set has character inputs, but", wl@learner.name, "does not support that!")
			}
			return(list(data=data, msg=msg))
		}
)
