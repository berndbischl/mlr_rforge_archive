#' @include wrapped.learner.regr.r 
roxygen()

#' @export
setClass(
		"kknn.knn.regr", 
		contains = c("wrapped.learner.regr")
)


#----------------- train.kknn.model ---------------------------------------------------------

train.kknn.model2 <- function(formula, data, ...) {
	model <- list(formula=formula, data=data, parset=list(...))
	class(model) <- "kknn"
	return(model)
}

predict.kknn.model2 <- function(model, newdata, ...) {
	# this is stupid but kknn forces it....
	cl <- as.character(model$formula)[2]
	newdata[,cl] <- 0
	
	pars <- list(formula=model$formula, train=model$data, test=newdata)  
	pars <- c(pars, model$parset)
	do.call(kknn, pars)$fitted.values
}


#----------------- constructor ---------------------------------------------------------

setMethod(
		f = "initialize",
		signature = signature("kknn.knn.regr"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- train.kknn.model2 
			predict.fct <- predict.kknn.model2
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="KKNN", learner.pack="kknn",
					learner.model.class="kknn", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars,
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					learner.props=desc)
			
			return(.Object)
		}
)



