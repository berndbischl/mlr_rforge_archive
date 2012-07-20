predict_nas = function(model, newdata) {
	if (model$learner$type == "classif") {
    levs = model$task.desc$class.levels  
		p = switch(model$learner$predict.type, 
				response = factor(rep(NA, nrow(newdata)), levels=levs),
				matrix(as.numeric(NA), nrow=nrow(newdata), ncol=length(levs), dimnames=list(NULL, levs))
		)
	} else {
		p = as.numeric(rep(NA, nrow(newdata)))
	}
	return(p)
}

#' @S3method print FailureModel
print.FailureModel = function(x, ...) {
  print.WrappedModel(x)
  catf("Training failed: ", x$learner.model)
}
