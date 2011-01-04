#' @include prediction.r
roxygen()
#' @include task.learn.r
roxygen()
#' @include resample.instance.r
roxygen()

#' @exportClass resample.prediction

setClass(
		"resample.prediction",
		contains = c("prediction"),
		representation = representation(
				instance="resample.instance" 
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("resample.prediction"),
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
		signature = signature("resample.prediction"),
		def = function(x) {
			return(
					paste(
							"Resampling result for: ", to.string(x@instance@desc),
							#"Learner models were ", ifelse(length(x@models)==0,"not", ""), " saved\n\n",
							#paste(capture.output(str(x@preds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)

#' Getter
#'
#' Note that in the case of the "prob", "response" and "decision"
#' fields, the results are returned in the order they were used by
#' the resampling strategy and not in the order present in the
#' dataset. This mainly applies to cross-validation were a different
#' order might be expected.
#'
#' @rdname resample.prediction-class
setMethod(
		f = "[",
		signature = signature("resample.prediction"),
		def = function(x,i,j,...,drop) {
			if (i == "iters")
				return(x@instance["iters"])
			callNextMethod()
		}
)

#' Converts object to a list of normal prediction objects - one for each iteration.
#' @rdname resample.prediction-class

setMethod(
		f = "as.list",
		signature = signature("resample.prediction"),
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
          test[[i]] = new("prediction", task.desc=x@desc, 
            type=x@type, df=subset(dfs[[i]], subset=(set=="test")), threshold=x@threshold, x@time)						
        if (has.train)
          train[[i]] = new("prediction", task.desc=x@desc, 
            type=x@type, df=subset(dfs[[i]], subset=(set=="train")), threshold=x@threshold, x@time)						
      }
      list(
        test = if (has.test) test else NULL,
        train = if (has.train) train else NULL
      )
		}
)


setAs("resample.prediction", "prediction", 
		function(from, to) {
			df = from@df
			df$iter = NULL
			new("prediction", task.desc=from@desc,  
					type=from@type, df=df, threshold=from@threshold, sum(from@time.fit), sum(from@time.predict))						
		}
)
