#' @include learner.props.r
#' @include wrapped.learner.r
roxygen()

#' Wrapped.learner.regr ---text!----
#' @slot train.par.for.classes  
#' @slot train.par.for.probs  
#' @slot predict.par.for.classes  
#' @slot predict.par.for.probs  
#' @slot trafo.for.classes  
#' @slot trafo.for.probs  
#' @slot dummy.classes Does the predict function need a class column in the dataframe for prediction? 
#' 		If TRUE but no class column is avaible in the data a null column is generated 
#'   	 	in predict (default is FALSE). 
#' 
#' @title wrapped.learner.regr

setClass(
		"wrapped.learner.regr",
		contains = c("wrapped.learner")
)
