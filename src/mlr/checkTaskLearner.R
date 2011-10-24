#' @include ClassifTask.R
roxygen()

checkTaskLearner = function(lt, learner) {
	data = getData(lt)
	msg = ""
	td = lt@desc
  if (is(lt, "ClassifTask") && learner@properties[["type"]] != "classif") {
    stop("Task is classification, but ", learner@id, " is for: ", learner@properties[["type"]])
  } else if (is(lt, "RegrTask") && learner@properties[["type"]] != "regr") {
    stop("Task is regression, but ", learner@id, " is for: ", learner@properties[["type"]])
  }
  if (td["has.missing"] && !learner@properties[["missings"]]) {
		stop("Data set has missing values, but ", learner@id, " does not support that!")
	}
	if (td@n.feat["numerics"] > 0 && !learner@properties[["numerics"]]) {
		stop("Data set has numeric inputs, but ", learner@id, " does not support that!")
	}
	if (td@n.feat["factors"] > 0 && !learner@properties[["factors"]]) {
		stop("Data set has factor inputs, but ", learner@id, " does not support that!")
	}
  if (is(lt, "ClassifTask") && length(td@class.levels)> 2 && !learner@properties[["multiclass"]]) {
    stop("Data set is a multiclass-problem, but ", learner@id, " does not support that!")
  }
  
	return(list(msg=msg))
}

