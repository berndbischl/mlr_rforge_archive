#' Create control structures for feature selection.
#' 
#' The following methods are available:
#'
#' \describe{
#'   \item{FeatSelControlExhaustive}{Exhaustive search. All feature sets (up to a certain size) are searched.}
#'   \item{FeatSelControlRandom}{Random search. Features vectors are randomly drawn.}
#'   \item{FeatSelControlSequential}{Deterministic forward or backward search.}
#'   \item{FeatSelControlGA}{Search via genetic algorithm.}
#' }
#' 
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is \code{TRUE}.
#' @param maxit [integer]\cr
#'   Maximal number of iterations. 
#' @param max.features [integer]\cr
#'   Maximal number of features. 
#' @param crossoverRate [numeric]\cr
#'   Parameter of the GA feature selection. Probability of choosing a bit from the first parent within the
#'   crossover mutation.
#' @param mutationRate [numeric]\cr
#'   Parameter of the GA feature selection. Probability of flipping a feature bit, i.e. switch from selecting a 
#'   feature to not selecting the feature and vice versa.
#' @param mu [integer]\cr
#'   Parameter of the GA feature selection. Size of the parent population.
#' @param lambda [integer]\cr
#'   Parameter of the GA feature selection. Size of the children population (should be smaller or equal to mu).
#' @param prob [numeric]\cr
#'   Parameter of the random feature selection. Probability of choosing a feature.
#' @param method [character]\cr
#'   Parameter of the sequential feature selection. A character representing the method. Possible values are 
#'   sfs (forward search), sbs (backward search), sffs (floating forward search), sfbs (floating backward search).
#' @param alpha [numeric]\cr
#'   Parameter of the sequential feature selection. Minimal value of improvement. 
#' @param beta [numeric]\cr
#'   Parameter of the sequential feature selection. Maximal value of setback. 
#'  
#' @return [\code{\link{FeatSelControl}}]. The specific subclass is one of
#'   \code{\link{FeatSelControlExhaustive}}, \code{\link{FeatSelControlRandom}}, \code{\link{FeatSelControlSequential}}, \code{\link{FeatSelControlGA}}.
#' @name FeatSelControl
#' @rdname FeatSelControl
#' @aliases FeatSelControlExhaustive FeatSelControlRandom FeatSelControlSequential FeatSelControlGA
NULL

makeFeatSelControl = function(cl, ...) {

  sel.func = switch(cl,
                    Random = makeFeatSelControlRandom,
                    Exhaustive = makeFeatSelControlExhaustive,
                    Sequential = makeFeatSelControlSequential,
                    GA = makeFeatSelControlGA
  )
  
  sel.func(...)
}

#S3method print FeatSelControl
print.FeatSelControl = function(x, ...) {
  catf("FeatSel control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  if (is.na(x$max.features))
    catf("Max. features: <not used>")
  else  
    catf("Max. features: %i", x$max.features)
  catf("Max. iterations: %i", x$maxit)
  #catf("Further arguments: %s", listToShortString(x$extra.args))
}

