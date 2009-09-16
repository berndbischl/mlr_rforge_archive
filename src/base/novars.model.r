#' Abstract base class for a model without variables.
#' @param targets Observed target values in training set. Used to base predictions on. 
setClass("novars.model", 
		representation = representation(
				targets = "ANY"
		)
)                                                     


#' Model without variables for classification.
setClass("novars.model.classif", 
		contains = c("novars.model")
)                                                     


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("novars.model.classif"),
		def = function(.Object, targets) {
			.Object@targets <- targets 
			return(.Object)
		}
)


#' Predict from model without variables for classification.
#' Either predict classes randomly weighted by class frequencies in the training set or return these frequencies
#' as estimated probabilities.  
setMethod(
		f = "predict",
		signature = signature(object="novars.model.classif"),
		def = function(object, newdata, type="class") {
			probs <- as.numeric(table(object@targets)) / length(object@targets)
			if (type == "prob")
				return(probs)
			sample(levels(object@targets), nrow(newdata), replace=TRUE, prob=probs)
		}
)

#' Model without variables for regression.
setClass("novars.model.regr", 
		contains = c("novars.model")
)                                                     


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("novars.model.regr"),
		def = function(.Object, targets) {
			.Object@targets <- targets 
			return(.Object)
		}
)


#' Predict constant from model without variables for regression.
#' At the moment only the mean is implemented.
setMethod(
		f = "predict",
		signature = signature(object="novars.model.regr"),
		def = function(object, newdata) {
			mean(targets)
		}
)




