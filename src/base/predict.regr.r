#' @include task.learn.r
#' @include wrapped.model.r
roxygen()



setMethod(
		f = "predict",
		signature = signature(object="regr.task"),
		def = function(object, model, newdata) {
			
			lt <- object
			wl <- lt@wrapped.learner

			if (missing(newdata)) {
				newdata <- lt@data[model@subset,]
			}
			
			g <- wl@predict.fct
			#	cn <- lt["class.name"]
			
			# was there an error in building the model? --> return NAs
#			if(class(model@learner.model)[1] == "learner.failure") {
#				p <- factor(rep(NA, nrow(newdata)), levels=lt["class.levels"])
#				return(p)
#			}
			
			g.pars <- c(list(model@learner.model, newdata=newdata), wl@predict.fct.pars)
			#print(str(g.pars))
			
			if(exists("debug.seed") && !is.null(debug.seed)) {
				set.seed(debug.seed)
				logger.warn("DEBUG SEED USED!!!!!!!!!!!!!!! REALLY SURE????")
			}
			
			logger.debug("Regr. predict:", wl@learner.name, "with pars:")
			logger.debug(wl@predict.fct.pars)
			logger.debug("on", nrow(newdata), "examples:")
			logger.debug(rownames(newdata))
			
			p <- do.call(g, g.pars)
			
			logger.debug("Regr. prediction:")
			logger.debug(p)
			return(p)
		}
)
