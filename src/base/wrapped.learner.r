#' @include learner.props.r
roxygen()



setClass(
		"wrapped.learner",
		representation(
				learner.name = "character",
				learner.pack = "character",
				train.fct = "function",
				train.fct.pars = "list",
				predict.fct = "function",
				predict.fct.pars = "list",
				learner.props = "learner.props"
		)
)


#---------------- constructor---- -----------------------------------------------------


setMethod(
		f = "initialize",
		signature = "wrapped.learner",
		def = function(.Object, learner.name, learner.pack, learner.model.class, learner.model.S4,
				train.fct, train.fct.pars=list(), 
				predict.fct=predict, predict.fct.pars=list(), 
				learner.props) {
			
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			
			if (missing(learner.name))
				return(.Object)
			
			if(!require(learner.pack, character.only=TRUE)) {
				stop(paste("Learn.task for", learner.name, "could not be constructed! package", learner.pack, "missing!"))
			}
			
			if (is.character(train.fct)) {
				train.fct <- tryCatch(
						eval(substitute(getFromNamespace(x, learner.pack), list(x=train.fct))),
						error=function(e) eval(substitute(get(x), list(x=train.fct))))
			}
			if (is.character(predict.fct)) {
				predict.fct <- tryCatch(
						eval(substitute(getFromNamespace(x, learner.pack), list(x=predict.fct))),
						error=function(e) eval(substitute(get(x), list(x=predict.fct))))
			}
			
			#if (!learner.model.S4) {
			#  setOldClass(learner.model.class, where=.GlobalEnv)
			#}
			#setIs(learner.model.class, "clr.external.model")      
			
			.Object@learner.name <- learner.name
			.Object@learner.pack <- learner.pack
			
			.Object@train.fct <- train.fct
			.Object@train.fct.pars <- train.fct.pars
			.Object@predict.fct <- predict.fct
			.Object@predict.fct.pars <- predict.fct.pars
			
			
			.Object@learner.props <- learner.props
			return(.Object)
		}
)


setMethod(
		f = "as.character",
		signature = "wrapped.learner",
		def = function(x) {
			return(
					as.character(x@learner.props)					
			)
		}
)

setMethod(
		f = "print",
		signature = "wrapped.learner",
		def = function(x, ...) {
			cat(as.character(x))
		}
)


setMethod(
		f = "show",
		signature = "wrapped.learner",
		def = function(object) {
			cat(as.character(object))
		}
)




