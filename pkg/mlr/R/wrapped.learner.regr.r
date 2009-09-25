#' @include learner.props.r
#' @include wrapped.learner.r
roxygen()

#' Wraps an already implemented regression method from R to make it accesible to mlr.
#' 
#' @title wrapped.learner.regr

setClass(
		"wrapped.learner.regr",
		contains = c("wrapped.learner")
)
