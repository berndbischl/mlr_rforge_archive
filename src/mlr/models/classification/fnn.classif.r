#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()

setClass(
  "classif.fnn", 
  contains = c("rlearner.classif")
)

#todo: probs can only be predicted for two class problems (winning class)

setMethod(
  f = "initialize",
  signature = signature("classif.fnn"),
  def = function(.Object) {
    
    # l is for reject option. cannot be done with mlr atm
    par.set = makeParameterSet(
      makeIntegerLearnerParameter(id="k", default=1L, lower=1L),
      makeLogicalLearnerParameter(id="use.all", default=TRUE, requires=expression(algorithm == "VR")),
      makeDiscreteLearnerParameter(id="algorithm", default="cover_tree", vals=list("cover_tree", "kd_tree", "VR"))
    )
  
    .Object = callNextMethod(.Object, pack="FNN", par.set=par.set)

    setProperties(.Object, 
      twoclass = TRUE,
      multiclass = TRUE,
      numerics = TRUE
    )
  }
)

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="classif.fnn", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    d = getData(.task, .subset, target.extra=TRUE)
    list(train=d, parset=list(...))
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "classif.fnn", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    m = .model@learner.model
    pars = list(train=m$train$data, test=.newdata, cl=m$train$target)  
    pars = c(pars, m$parset, list(...))
    p = do.call(FNN::knn, pars)
    attr(p, "nn.index") = NULL
    attr(p, "nn.dist") = NULL
    return(p)
  }
)	




