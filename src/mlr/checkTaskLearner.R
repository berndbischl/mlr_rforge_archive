#' @include ClassifTask.R
roxygen()

checkTaskLearner <- function(lt, learner) {
	ld <- learner@desc
	data <- lt["data"]
	msg <- ""
	td <- lt@desc
	
	if (td["has.missing"] && !ld@missings) {
		stop("Data set has missing values, but", learner@desc@id, "does not support that!")
	}
	if (td@n.feat["numerics"] > 0 && !ld@feat["numerics"]) {
		stop("Data set has numeric inputs, but", learner@desc@id, "does not support that!")
	}
	if (td@n.feat["factors"] > 0 && !ld@feat["factors"]) {
		stop("Data set has factor inputs, but", learner@desc@id, "does not support that!")
	}
  if (is(lt, "ClassifTask") && length(getClassLevels(td))> 2 && !ld@classes["multiclass"]) {
    stop("Data set is a multiclass-problem, but", learner@desc@id, "does not support that!")
  }
  
	return(list(msg=msg))
}

