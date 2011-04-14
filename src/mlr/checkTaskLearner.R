#' @include ClassifTask.R
roxygen()

checkTaskLearner <- function(lt, learner) {
	data <- lt["data"]
	msg <- ""
	td <- lt@desc
	
	if (td["has.missing"] && learner@properties[["missings"]]) {
		stop("Data set has missing values, but ", learner@id, " does not support that!")
	}
	if (td@n.feat["numerics"] > 0 && !learner@properties[["numerics"]]) {
		stop("Data set has numeric inputs, but ", learner@id, " does not support that!")
	}
	if (td@n.feat["factors"] > 0 && !learner@properties[["factors"]]) {
		stop("Data set has factor inputs, but ", learner@id, " does not support that!")
	}
  if (is(lt, "ClassifTask") && length(getClassLevels(td))> 2 && !learner@properties[["multiclass"]]) {
    stop("Data set is a multiclass-problem, but ", learner@id, " does not support that!")
  }
  
	return(list(msg=msg))
}

