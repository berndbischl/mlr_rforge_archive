#' @include learner.props.r
#' @include wrapped.learner.r
roxygen()

#' Wraps an already implemented classification method from R to make it accesible to mlr.
#' 
#' @title wrapped.learner.classif

setClass(
		"wrapped.learner.classif",
		contains = c("wrapped.learner")
)

