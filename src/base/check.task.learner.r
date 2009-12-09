#' @include task.classif.r
roxygen()

check.task.learner <- function(lt, learner) {
	wl <- learner
	ld <- wl@learner.props
	data <- lt@data
	msg <- ""
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
	return(list(msg=msg))
}

check.task.learner.classif <- function(lt, learner) {
	msg <- check.task(lt, learner)

	ld <- learner@learner.props
	dd <- lt@data.desc
	
	if (dd@class.nr > 2 && !ld@supports.multiclass) {
		msg <- paste("Data set is a multiclass-problem, but", learner@learner.name, "does not support that!")
	}
	return(list(msg=msg))
}


