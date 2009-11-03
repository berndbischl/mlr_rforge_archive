make.metacost.wrapper <- function(ct, costs, bs.iters=10, oob=TRUE) {
			
}


setMethod(
		f = "train.learner",
		
		signature = c(
				wrapped.learner="kernlab.svm.classif", 
				target="character", 
				data="data.frame", 
				weights="numeric", 
				parset="list"
		),
		
			
			# try to predict probs if supported
			ct <- make.classif.task(wrapped.learner, data=data, target=target)
			
			N <- nrow(ct["data"])
			lev <- ct["class.levels"]
			ngroup <- ct["class.nr"] 
			
			# maybe allow to draw smaller bs sample with n elements
			res.i = make.bs.instance(size=N, iters=m)
			
			rf <- resample.fit(ct, res.i, parset=parset, models=TRUE)
			# maybe predict all example, not only oob

			
			models <- rf@models
			ps <- lapply(models, function(y) predict(ct, y))
			
			psum <- function(pmodels, N, ngroup, resample, m) {
				post <- matrix(0, N, ngroup)
				if(q) {
					resample.2 <- apply(resample, 2, function(i) !1:N %in% i)
					m.n <- rowSums(resample.2)
					for(j in seq(along = pmodels)) post[resample.2[,j],] <- post[resample.2[,j],] + pmodels[[j]][resample.2[,j],]
					return(post/m.n)
				} else {
					for(j in seq(along = pmodels)) post <- post + pmodels[[j]]
					return(post/m)        
				}
			}
			if(p) {
				posterior <- psum(pmodels, N, ngroup, resample, m)
			} else {
				pmodels <- lapply(pmodels, function(y) diag(ngroup)[y,])
				posterior <- psum(pmodels, N, ngroup, resample, m)
			}
			
			
			
			label <- factor(lev[max.col(posterior %*% t(C))], levels = lev)
		}
)



