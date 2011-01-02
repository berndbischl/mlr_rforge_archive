#' Base class for description of resampling algorithms.
#' A description of a resampling algorithm contains all necessary information to 
#' create a \code{\linkS4class{resample.instance}}, when given the size of the data set.
#' For construction simply use the factory method \code{\link{make.res.desc}}. 
#' 
#' Getter.
#' 
#' \describe{
#' 	\item{instance.class [character]}{S4 class name of the corresponding resample.instance}
#' 	\item{id [string]}{Name of resampling strategy}
#' 	\item{iters [numeric]}{Number of iterations. Note that this the complete number of generated train/test sets, so for a 10 times repeated 5fold cross-validation it would be 50.}
#'  \item{predict [string]}{What to predict during resampling: "train", "test" or "both" sets.}
#' } 
#' @exportClass resample.desc 
#' @title resample.desc

setClass(
		"resample.desc", 
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
		signature = signature("resample.desc"),
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
		signature = signature("resample.desc"),
		def = function(x) {
			return(
					paste(
							x["id"], " with ", x@iters, " iterations.\n",	
              "Predict: ", x["predict"], 
              sep=""
          )
			)
		}
)


setClass(
		"resample.desc.seq", 
		contains = c("resample.desc")
)


setClass(
		"resample.desc.nonseq", 
		contains = c("resample.desc")
)



