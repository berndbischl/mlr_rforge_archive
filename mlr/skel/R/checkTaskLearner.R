checkTaskLearner = function(task, learner) {
	td = task$desc
  if (td$desc$type != learner$properties[["type"]]) 
    stopf("Task is %s, but learner %s is for %s!", td$id, learner$id, learner$properties[["type"]])
  if (td$has.missing && !learner$properties[["missings"]])
		stopf("Task %s has missing values, but learner %s does not support that!", td$id, learner$id)
	if (td$n.feat["numerics"] > 0 && !learner$properties[["numerics"]]) 
    stopf("Task %s has numeric inputs, but learner %s does not support that!", td$id, learner$id)
	if (td$n.feat["factors"] > 0 && !learner$properties[["factors"]])
    stopf("Data set has factor inputs, but learner %s does not support that!", td$id, learner$id)
  if (td$desc$type == "classif" && length(td$class.levels)> 2 && !learner$properties[["multiclass"]])
    stopf("Task %s is a multiclass-problem, but learner %s does not support that!", td$id, learner$id)
}

