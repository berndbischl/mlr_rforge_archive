# todo: remove iters from some desc. we also can use length of train.inds in instance
#' Base class for description of resampling algorithms.
#' A description of a resampling algorithm contains all necessary information to 
#' create a \code{\linkS4class{resample.instance}}, when given the size of the data set.
#' For construction simply use the factory method \code{\link{makeResampleDesc}}. 
#' 
#' @slot instance.class S4 class name of the corresponding resample.instance. 
#' @slot id Name of resampling strategy. 
#' @slot iters Number of iterations. Note that this is always the complete number of generated train/test sets, so for a 10 times repeated 5fold cross-validation it would be 50. 
#' @slot predict What to predict during resampling: 'train', 'test' or 'both' sets.
#'  
#' @exportClass ResampleDesc 
#' @title Base class for description of resampling algorithms.

setClass(
		"ResampleDesc", 
		contains = c("object"),
		representation = representation(
				instance.class = "character", 
				id = "character", 
				iters = "integer",
        predict = "character"
		)
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("ResampleDesc"),
		def = function(.Object, instance.class, id, iters, predict="test") {
			if (missing(id))
				return(.Object)					
			.Object@instance.class = instance.class
			.Object@id = id
			.Object@iters = iters
      .Object@predict = predict
      return(.Object)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("ResampleDesc"),
		def = function(x) {
			return(
					paste(
							x@id, " with ", x@iters, " iterations.\n",	
              "Predict: ", x["predict"], 
              sep=""
          )
			)
		}
)


setClass(
		"ResampleDesc.seq", 
		contains = c("ResampleDesc")
)


setClass(
		"ResampleDesc.nonseq", 
		contains = c("ResampleDesc")
)



