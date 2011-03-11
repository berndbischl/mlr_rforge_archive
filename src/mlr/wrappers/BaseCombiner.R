#' @include Learner.R
roxygen()
#' @include LearnTask.R
roxygen()
#' @include setId.R
roxygen()
#' @include setHyperPars.R
roxygen()
#' @include setPredictType.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include WrappedModel.R
roxygen()


#' Abstract base class to wrap something around a learner.
#' @exportClass BaseCombiner

setClass(
  "BaseCombiner",
  contains = c("Learner"),
  representation = representation(
    learners = "list"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("BaseCombiner"),
  def = function(.Object, learners, id, pack=character(0), par.set, par.vals=list()) {
    if (missing(learners))
      return(make.empty(.Object))
    .Object@learners = learners
    callNextMethod(.Object, id=id, pack=pack, par.set=par.set, par.vals=par.vals)
  }
)


#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="BaseCombiner", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    lapply(.learner@learners, function(w) train(w, .task, .subset))
  }
)



##' @rdname to.string
#setMethod(f = "to.string",
#  signature = signature("BaseCombiner"),
#  def = function(x) {
#    s = ""
#    y = x 
#    while (is(y, "BaseCombiner")) {
#      s = paste(s, class(y), "->", sep="")
#      y = y@learner
#    }
#    s = paste(s, class(y))
#    
#    return(paste(
#        s, "\n",
#      ))
#  })



