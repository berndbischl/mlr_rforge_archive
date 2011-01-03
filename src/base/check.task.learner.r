#' @include task.classif.r
roxygen()

check.task.learner <- function(lt, learner) {
	wl <- learner
	data <- lt["data"]
	msg <- ""
	td <- lt@task.desc
	
	if (td["has.missing"] && !wl["missings"]) {
		msg <- paste("Data set has missing values, but", wl["id"], "does not support that!")
	}
	if (td["n.feat"]["double"] > 0 && !wl["doubles"]) {
		msg <- paste("Data set has numeric inputs, but", wl["id"], "does not support that!")
	}
	if (td["n.feat"]["fact"] > 0 && !wl["factors"]) {
		msg <- paste("Data set has factor inputs, but", wl["id"], "does not support that!")
	}
	return(list(msg=msg))
}

check.task.learner.classif <- function(lt, learner) {
	msg <- check.task.learner(lt, learner)

	td <- lt@task.desc
	
	if (td["class.nr"]> 2 && !learner["multiclass"]) {
		msg <- paste("Data set is a multiclass-problem, but", learner["id"], "does not support that!")
	}
	return(list(msg=msg))
}


