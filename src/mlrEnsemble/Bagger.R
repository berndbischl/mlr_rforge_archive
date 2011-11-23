#' @exportClass Bagger
setClass(
  "Bagger",   
  contains = c("BaseWrapper"),
  representation = representation(
    resampling = "ResampleDesc"
  )
)   

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("Bagger"),
  def = function(.Object, learner) {
    if (missing(learner))
      return(make.empty(.Object))
    #.Object@resampling = resampling
    .Object = callNextMethod(.Object, learner, par.set=makeParamSet(), par.vals=list())
    # set predict type of base learner
    setPredictType(.Object, learner@predict.type)
  }
)


#' @export 
makeBagger = function(learner) {
  if (is.character(learner))
    learner = makeLearner(learner)
  new("Bagger", learner=learner)
}

#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="Bagger", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    .task = subsetData(.task, subset=.subset)
    # for now simply partition into 10 parts
    # we neeed a res. class for "Partitioning" and we can use "Subsampling" 
    # check in constructor that nothing else is allowed
    rin = makeResampleInstance(makeResampleDesc("CV", iters=10), task=.task)
    iters = rin@desc@iters
    bl = .learner@learner
    f = function(i, task, rin) {
      # use test indices of CV
      inds = rin@test.inds[[i]]
      train(bl, .task, subset=inds)
    }
    models = mylapply(1:iters, f, task=.task, rin=rin)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "Bagger", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model@learner.model
    p = mylapply(models, function(m) predict(m, newdata=.newdata, ...))
    #todo maybe we want to predict probs with base learner but use those to get labels?
    if (.type == "response") {
      p = lapply(p, function(x) as.character(x@df$response))
      p = do.call(cbind, p)
      p = apply(p, 1, vote.majority)
      return(as.factor(p))
    } else if (.type == "prob") {
      # todo
    }
  }
) 

