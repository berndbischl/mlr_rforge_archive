#' @include Prediction.R
roxygen()
#' @include LearnTask.R
roxygen()
#' @include ResampleInstance.r
roxygen()


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
			type = p1["type"]
      df = data.frame()
      for (i in 1:instance["iters"]) {
        df = rbind(df, cbind(preds.test[[i]]@df, iter=i, set="test"))
        if (!is.null(preds.train[[i]]))
          df = rbind(df, cbind(preds.train[[i]]@df, iter=i, set="train"))                 
      }
      
      threshold = p1["threshold"]

      tp = sapply(preds.test, function(x) x["time"])

			.Object@type = type			
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

#' Getter
#'
#'
#' @rdname ResamplePrediction-class
setMethod(
		f = "[",
		signature = signature("ResamplePrediction"),
		def = function(x,i,j,...,drop) {
			if (i == "iters")
				return(x@instance["iters"])
			callNextMethod()
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
      for(i in 1:x@instance["iters"]) {
        if (has.test)
          test[[i]] = new("Prediction", task.desc=x@desc, 
            type=x@type, df=subset(dfs[[i]], subset=(set=="test")), threshold=x@threshold, x@time)						
        if (has.train)
          train[[i]] = new("Prediction", task.desc=x@desc, 
            type=x@type, df=subset(dfs[[i]], subset=(set=="train")), threshold=x@threshold, x@time)						
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
					type=from@type, df=df, threshold=from@threshold, sum(from@time.fit), sum(from@time))						
		}
)
