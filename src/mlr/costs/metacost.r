#make.metacost.wrapper <- function(ct, costs, bs.iters=10, oob=TRUE) {
#			
#}
#
#
#setMethod(
#		f = "train.learner",
#		
#		signature = c(
#				wrapped.learner="classif.ksvm", 
#				target="character", 
#				data="data.frame", 
#				weights="numeric", 
#				parset="list"
#		),
#		
#			
#			# try to predict probs if supported
#			ct <- make.task(wrapped.learner, data=data, target=target)
#			
#			N <- nrow(ct["data"])
#			lev <- ct["class.levels"]
#			ngroup <- ct["class.nr"] 
#			
#			# maybe allow to draw smaller bs sample with n elements
#			res.i = make.bs.instance(size=N, iters=m)
#			
#			rf <- resample(ct, res.i, parset=parset, models=TRUE)
#			# maybe predict all example, not only oob
#
#			
#			models <- rf@models
#			ps <- lapply(models, function(y) predict(ct, y))
#			
#			psum <- function(pmodels, N, ngroup, resample, m) {
#				post <- matrix(0, N, ngroup)
#				if(q) {
#					resample.2 <- apply(resample, 2, function(i) !1:N %in% i)
#					m.n <- rowSums(resample.2)
#					for(j in seq(along = pmodels)) post[resample.2[,j],] <- post[resample.2[,j],] + pmodels[[j]][resample.2[,j],]
#					return(post/m.n)
#				} else {
#					for(j in seq(along = pmodels)) post <- post + pmodels[[j]]
#					return(post/m)        
#				}
#			}
#			if(p) {
#				posterior <- psum(pmodels, N, ngroup, resample, m)
#			} else {
#				pmodels <- lapply(pmodels, function(y) diag(ngroup)[y,])
#				posterior <- psum(pmodels, N, ngroup, resample, m)
#			}
#			
#			
#			
#			label <- factor(lev[max.col(posterior %*% t(C))], levels = lev)
#		}
#)
#
#
#


##' @include learnerR.r
#roxygen()
##' @include WrappedModel.R
#roxygen()
##' @include train.learner.r
#roxygen()
##' @include pred.learner.r
#roxygen()
#
#setClass(
#		"metacost", 
#		contains = c("rlearner.classif"),
#		representation = representation(
#			learner = "rlearner",	
#			iters = "numeric",
#			base.probs = "logical",
#			all = "logical"	
#		)
#)
#
#
#
#setMethod(
#		f = "initialize",
#		signature = signature("metacost"),
#		def = function(.Object, learner, iters, base.probs, all) {
#			.Object@learner <- makeLearner(learner) 
#			.Object@iters <- iters 
#			.Object@base.probs <- base.probs
#			.Object@all <- all 
#			return(.Object)
#		}
#)
#
##make.metacost.wrapper <- function(ct, costs, bs.iters=10, all=TRUE) {
##	
##}
#
##' @rdname train.learner
#
#setMethod(
#		f = "train.learner",
#		
#		signature = c(
#				wrapped.learner="metacost", 
#				target="character", 
#				data="data.frame", 
#				weights="numeric", 
#				costs="matrix", 
#				parset="list"
#		),
#		
#		def = function(wrapped.learner, target, data, weights, costs, parset) {
#			
#			ct <- make.task(class(wrapped.learner@learner), data=data, target=target)
#			
#			N <- ct["size"]
#					
#			# maybe allow to draw smaller bs sample with n elements
#			res.i = make.bs.instance(size=N, iters=wrapped.learner@iters)
#			
#			# dont pred here, coz we do this later
#			rf <- resample(ct, res.i, , models=TRUE)
#			models <- rf@models
#
#			used = numeric(N)
#			dummy = diag(ct["class.nr"])
#			rownames(dummy) = ct["class.levels"]
#			colnames(dummy) = ct["class.levels"]
#			result = matrix(0, N, ct["class.nr"])
#			if (wrapped.learner@base.probs) {
#				type = "prob"
#			} else {
#				type = "class"
#			}
#			
#			for (i in 1:wrapped.learner@iters){		
#				if (wrapped.learner@all)
#					inds = 1:N
#				else
#					inds = res.i["test.inds", i]
#				
#				ps <- predict(models[[i]], newdata=ct["data", row=inds], type=type)
#				if (!wrapped.learner@base.probs)
#					ps <- dummy[ps,]
#				if (i==1)
#					colnames(result) = colnames(ps)
#				result[inds,] = result[inds,] + ps
#				used[inds] = used[inds] + 1
#			}
#			result = diag(1/used) %*% result
#			# make probs equal if never predicted
#			result[used==0, ] = 1/ct["class.nr"]
#			cs = result %*% t(costs)
#			labs = max.col(-cs)
#			labs = as.factor(colnames(result)[labs])
#			data[,ct["target.col"]] = labs
#			print(result)
#			print(labs)
#			return(
#					train.learner(wrapped.learner@learner, target, data, weights, matrix(0,0,0), parset)
#			)
#		}
#	
#)
#
#



