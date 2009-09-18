#' @include task.classif.r
roxygen()

#setGeneric(
#		name = "check.task.classif",
#		def = function(lt) {
#			standardGeneric("check.task.classif")
#		}
#)
#
#setMethod(
#		f = "check.task.classif",
#		signature = signature(lt="classif.task"),
#		def = function(lt) {
#			wl <- lt@wrapped.learner
#			ld <- wl@learner.props
#			data <- lt@data
#			msg <- ""
#			data <- prep.classif.data(data=data, data.desc=lt@data.desc, ints.as.nums=T)
#			lt@data.desc <- make.data.desc(data=data, target.col = lt@data.desc@target.col)
#			dd <- lt@data.desc
#					
#			if (dd@class.nr > 2 && !ld@supports.multiclass) {
#				msg <- paste("Data set is a multiclass-problem, but", wl@learner.name, "does not support that!")
#			}
#			if (dd@has.missing && !ld@supports.missing) {
#				msg <- paste("Data set has missing values, but", wl@learner.name, "does not support that!")
#			}
#			if (dd@numerics > 0 && !ld@supports.numerics) {
#				msg <- paste("Data set has numeric inputs, but", wl@learner.name, "does not support that!")
#			}
#			if (dd@factors > 0 && !ld@supports.factors) {
#				msg <- paste("Data set has factor inputs, but", wl@learner.name, "does not support that!")
#			}
#			if (dd@characters > 0 && !ld@supports.characters) {
#				msg <- paste("Data set has character inputs, but", wl@learner.name, "does not support that!")
#			}
#			return(list(data=data, msg=msg))
#		}
#)

check.task.classif <- function(lt) {
	wl <- lt@wrapped.learner
	ld <- wl@learner.props
	data <- lt@data
	msg <- ""
	data <- prep.classif.data(data=data, data.desc=lt@data.desc, ints.as.nums=T)
	lt@data.desc <- make.data.desc(data=data, target.col = lt@data.desc@target.col)
	dd <- lt@data.desc
	
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

