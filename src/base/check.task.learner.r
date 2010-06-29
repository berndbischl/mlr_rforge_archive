#' @include task.classif.r
roxygen()

check.task.learner <- function(lt, learner) {
	wl <- learner
	data <- lt["data"]
	msg <- ""
	dd <- lt@data.desc
	
	if (dd["class.nr"] > 2 && !wl["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", wl["id"], "does not support that!")
	}
	if (dd["has.missing"] && !wl["missings"]) {
		msg <- paste("Data set has missing values, but", wl["id"], "does not support that!")
	}
	if (dd["numerics"] > 0 && !wl["numerics"]) {
		msg <- paste("Data set has numeric inputs, but", wl["id"], "does not support that!")
	}
	if (dd["factors"] > 0 && !wl["factors"]) {
		msg <- paste("Data set has factor inputs, but", wl["id"], "does not support that!")
	}
	if (dd["characters"] > 0 && !wl["characters"]) {
		msg <- paste("Data set has character inputs, but", wl["id"], "does not support that!")
	}
	if (any(is.na(lt["targets"]))) {
		msg <- paste("Target values contain missings!")
	}
	return(list(msg=msg))
}

check.task.learner.classif <- function(lt, learner) {
	msg <- check.task.learner(lt, learner)

	dd <- lt@data.desc
	
	if (dd["class.nr"]> 2 && !learner["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", learner["id"], "does not support that!")
	}
	return(list(msg=msg))
}


