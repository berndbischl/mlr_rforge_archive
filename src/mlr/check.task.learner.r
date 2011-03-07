#' @include ClassifTask.R
roxygen()

check.task.learner <- function(lt, learner) {
	ld <- learner@desc
	data <- lt["data"]
	msg <- ""
	td <- lt@desc
	
	if (td["has.missing"] && !ld@missings) {
		msg <- paste("Data set has missing values, but", wl@desc@id, "does not support that!")
	}
	if (td["n.feat"]["double"] > 0 && !ld@feat["doubles"]) {
		msg <- paste("Data set has numeric inputs, but", wl@desc@id, "does not support that!")
	}
	if (td["n.feat"]["fact"] > 0 && !ld@feat["factors"]) {
		msg <- paste("Data set has factor inputs, but", wl@desc@id, "does not support that!")
	}
	return(list(msg=msg))
}

check.task.learner.classif <- function(lt, learner) {
	msg <- check.task.learner(lt, learner)

	td <- lt@desc
  ld <- learner@desc
  
	if (length(getClassLevels(td))> 2 && !ld@classes["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", learner@desc@id, "does not support that!")
	}
	return(list(msg=msg))
}


