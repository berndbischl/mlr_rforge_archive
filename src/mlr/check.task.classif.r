#' @include ClassifTask.R
roxygen()


# empty class levels
# feature types
# column names
# missings
# infs
# check task learner raus?


check.task <- function(lt, learner) {
	wl <- learner
	ld <- wl@desc
	data <- lt["data"]
	msg <- ""
	td <- lt@desc
	
	if (length(getClassLevels(td)) > 2 && !ld["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", wl@desc@id, "does not support that!")
	}
	if (td["has.missing"] && !ld["missings"]) {
		msg <- paste("Data set has missing values, but", wl@desc@id, "does not support that!")
	}
	if (td@n.feat["numerics"] > 0 && !ld@feat["numerics"]) {
		msg <- paste("Data set has numeric inputs, but", wl@desc@id, "does not support that!")
	}
	if (td@n.feat["factors"] > 0 && !ld@feat["factors"]) {
		msg <- paste("Data set has factor inputs, but", wl@desc@id, "does not support that!")
	}
	return(list(msg=msg))
}

check.task.classif <- function(lt, learner) {
	msg <- check.task(lt, learner)

	ld <- learner@desc
	td <- lt@desc
	
	if (length(getClassLevels(td)) > 2 && !ld@classes["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", learner@desc@id, "does not support that!")
	}
	return(list(msg=msg))
}


