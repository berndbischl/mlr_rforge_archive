#' @include Prediction.R
roxygen()
#' @include LearnTask.R
roxygen()
#' @include ResampleInstance.R
roxygen()

#' Contains predictions from resampling, returned (among other stuff) by function \code{\link{resample}}.
#' Can basically be used as its super class.
#' The main differences are:
#' (a) The internal data.frame (slot \code{df}) contains an additional column \code{id}, specifying the iteration
#' of the resampling strategy. (b) The object can be converted into a list of  \code{\linkS4class{Prediction}} objects by using \code{as.list} on it,
#' one object for each resampling iteration.
#' @slot instance Resampling instance that was used to generate the prediction. 
#' @exportClass ResamplePrediction
#' @title Prediction from resampling.
#' @seealso \code{\link{resample}}, \code{\link{predict}} 

setClass(
		"ResamplePrediction",
		contains = c("Prediction"),
		representation = representation(
				instance="ResampleInstance" 
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("ResamplePrediction"),
		def = function(.Object, instance, preds.test, preds.train) {
			p1 = preds.test[[1]]
			.Object@instance = instance
			predict.type = p1@predict.type
      df = data.frame()
      for (i in 1:instance@desc@iters) {
        df = rbind(df, cbind(preds.test[[i]]@df, iter=i, set="test"))
        if (!is.null(preds.train[[i]]))
          df = rbind(df, cbind(preds.train[[i]]@df, iter=i, set="train"))                 
      }
      
      threshold = p1["threshold"]

      tp = sapply(preds.test, function(x) x["time"])

			.Object@predict.type = predict.type			
			.Object@df = df			
			.Object@threshold = threshold			
			.Object@desc = p1@desc	
			.Object@time = tp
			return(.Object)
		}
)




#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("ResamplePrediction"),
		def = function(x) {
			return(
					paste(
							"Resampled Prediction for: ", to.string(x@instance@desc),
							#"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
							#paste(capture.output(str(x@preds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)


#' Converts object to a list of a \code{\linkS4class{Prediction}} objects - one for each iteration.
#' @rdname ResamplePrediction-class

setMethod(
		f = "as.list",
		signature = signature("ResamplePrediction"),
		def = function(x, all.names = FALSE, ...) {
			df = x@df
			iter = as.factor(df$iter)
			df = subset(df, select=-iter)
			dfs = split(df, iter) 
			test = train = list()
      has.train = "train" %in% levels(df$set)
      has.test = "test" %in% levels(df$set)
      for(i in 1:x@instance@desc@iters) {
        if (has.test)
          test[[i]] = new("Prediction", task.desc=x@desc, 
            predict.type=x@predict.type, df=subset(dfs[[i]], subset=(set=="test")), threshold=x@threshold, x@time)						
        if (has.train)
          train[[i]] = new("Prediction", task.desc=x@desc, 
            predict.type=x@predict.type, df=subset(dfs[[i]], subset=(set=="train")), threshold=x@threshold, x@time)						
      }
      list(
        test = if (has.test) test else NULL,
        train = if (has.train) train else NULL
      )
		}
)

#tests!

setAs("ResamplePrediction", "Prediction", 
		function(from, to) {
			df = from@df
			df$iter = NULL
			new("Prediction", task.desc=from@desc,  
					predict.type=from@predict.type, df=df, threshold=from@threshold, sum(from@time.fit), sum(from@time))						
		}
)
