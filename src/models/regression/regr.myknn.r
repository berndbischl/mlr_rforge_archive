#' @include wrapped.learner.regr.r
roxygen()

#' @export
setClass(
		"myknn.regr", 
		contains = c("wrapped.learner.regr")
)


#----------------- train.kknn.model ---------------------------------------------------------

train.myknn <- function(target, data, k=1, fun=mean) {
	list(target=target, data=data, k=k, fun=fun)
}

predict.myknn <- function(model, newdata) {
	k = model$k
	mf.train <- model.frame(model$target, data=model$data) 
	mm.train <- model.matrix(model$target, data=mf.train) 
	y <- esponse(mf.train)
	mm.test <- model.matrix(terms(model$target, data=newdata), data=newdata) 
	nns <- ann(mm.train, mm.test, k=k)$knnIndexDist[,1:k, drop=FALSE]
	p <- apply(nns, 1, function(x) model$fun(y[x]) ) 
	
}


#----------------- constructor ---------------------------------------------------------

setMethod(
		f = "initialize",
		signature = signature("myknn.regr"),
		def = function(.Object, data, target) {
			
			desc = new("method.desc",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="myknn", learner.pack="yaImpute", learner.props=desc, ...)
			return(.Object)
		}
)



