#' @include task.classif.r
roxygen()

check.task <- function(lt, learner) {
	wl <- learner
	ld <- wl@desc
	data <- lt["data"]
	msg <- ""
	dd <- lt@data.desc
	
	if (dd["class.nr"] > 2 && !ld["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", wl["id"], "does not support that!")
	}
	if (dd["has.missing"] && !ld["missings"]) {
		msg <- paste("Data set has missing values, but", wl["id"], "does not support that!")
	}
	if (dd["n.num"] > 0 && !ld["numerics"]) {
		msg <- paste("Data set has numeric inputs, but", wl["id"], "does not support that!")
	}
	if (dd["n.fact"] > 0 && !ld["factors"]) {
		msg <- paste("Data set has factor inputs, but", wl["id"], "does not support that!")
	}
	if (dd["n.char"] > 0 && !ld["characters"]) {
		msg <- paste("Data set has character inputs, but", wl["id"], "does not support that!")
	}
	return(list(msg=msg))
}

check.task.classif <- function(lt, learner) {
	msg <- check.task(lt, learner)

	ld <- learner@desc
	dd <- lt@data.desc
	
	if (dd["class.nr"] > 2 && !ld["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", learner["id"], "does not support that!")
	}
	return(list(msg=msg))
}


