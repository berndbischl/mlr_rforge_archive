#' @include task.classif.r
roxygen()

check.task <- function(lt, learner) {
	wl <- learner
	ld <- wl@desc
	data <- lt["data"]
	msg <- ""
	td <- lt@desc
	
	if (td["class.nr"] > 2 && !ld["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", wl["id"], "does not support that!")
	}
	if (td["has.missing"] && !ld["missings"]) {
		msg <- paste("Data set has missing values, but", wl["id"], "does not support that!")
	}
	if (td["n.feat"]["double"] > 0 && !ld["doubles"]) {
		msg <- paste("Data set has numeric inputs, but", wl["id"], "does not support that!")
	}
	if (td["n.feat"]["fact"] > 0 && !ld["factors"]) {
		msg <- paste("Data set has factor inputs, but", wl["id"], "does not support that!")
	}
	return(list(msg=msg))
}

check.task.classif <- function(lt, learner) {
	msg <- check.task(lt, learner)

	ld <- learner@desc
	td <- lt@desc
	
	if (td["class.nr"] > 2 && !ld["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", learner["id"], "does not support that!")
	}
	return(list(msg=msg))
}


