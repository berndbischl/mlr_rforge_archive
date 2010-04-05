#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()

setClass(
		"metacost", 
		contains = c("wrapped.learner.classif"),
		representation = representation(
			learner = "wrapped.learner",	
			iters = "numeric",
			base.probs = "logical",
			all = "logical"	
		)
)



setMethod(
		f = "initialize",
		signature = signature("metacost"),
		def = function(.Object, learner, iters, base.probs, all) {
			.Object@learner <- make.learner(learner) 
			.Object@iters <- iters 
			.Object@base.probs <- base.probs
			.Object@all <- all 
			return(.Object)
		}
)

#make.metacost.wrapper <- function(ct, costs, bs.iters=10, all=TRUE) {
#	
#}


setMethod(
		f = "train.learner",
		
		signature = c(
				wrapped.learner="metacost", 
				target="character", 
				data="data.frame", 
				weights="numeric", 
				costs="matrix", 
				parset="list"
		),
		
		def = function(wrapped.learner, target, data, weights, costs, parset) {
			
			ct <- make.classif.task(class(wrapped.learner@learner), data=data, target=target)
			
			N <- ct["size"]
					
			# maybe allow to draw smaller bs sample with n elements
			res.i = make.bs.instance(size=N, iters=wrapped.learner@iters)
			
			# dont pred here, coz we do this later
			rf <- resample.fit(ct, res.i, parset=parset, models=TRUE)
			models <- rf@models

			used = numeric(N)
			dummy = diag(ct["class.nr"])
			rownames(dummy) = ct["class.levels"]
			colnames(dummy) = ct["class.levels"]
			result = matrix(0, N, ct["class.nr"])
			if (wrapped.learner@base.probs) {
				type = "prob"
			} else {
				type = "class"
			}
			
			for (i in 1:wrapped.learner@iters){		
				if (wrapped.learner@all)
					inds = 1:N
				else
					inds = res.i["test.inds", i]
				
				ps <- predict(models[[i]], newdata=ct["data", inds], type=type)
				if (!wrapped.learner@base.probs)
					ps <- dummy[ps,]
				if (i==1)
					colnames(result) = colnames(ps)
				result[inds,] = result[inds,] + ps
				used[inds] = used[inds] + 1
			}
			result = diag(1/used) %*% result
			# make probs equal if never predicted
			result[used==0, ] = 1/ct["class.nr"]
			cs = result %*% t(costs)
			labs = max.col(-cs)
			labs = as.factor(colnames(result)[labs])
			data[,ct["target.col"]] = labs
			print(result)
			print(labs)
			return(
					train.learner(wrapped.learner@learner, target, data, weights, matrix(0,0,0), parset)
			)
		}
	
)



