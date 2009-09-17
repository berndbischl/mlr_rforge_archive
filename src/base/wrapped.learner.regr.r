#' @include learner.props.r
#' @include wrapped.learner.r
roxygen()

#' Wrapped.learner.regr ---KORREKTURLESEN----
#' @slot train.par.for.classes  List of parameters for training (classification)
#' @slot train.par.for.probs  	List of parameters for training (regression)
#' @slot predict.par.for.classes	List of parameters for predicting (classification)
#' @slot predict.par.for.probs  	List of parameters for predicting (regression)
#' @slot trafo.for.classes  		Function, specifying a transformation for the classes
#' @slot trafo.for.probs  			Function, specifying a transformation for probabilities
#' @slot dummy.classes Does the predict function need a class column in the dataframe for prediction? 
#' 		If TRUE but no class column is avaible in the data a null column is generated 
#'   	 	in predict (default is FALSE). 
#' 
#' @title wrapped.learner.regr

setClass(
		"wrapped.learner.regr",
		contains = c("wrapped.learner")
)
