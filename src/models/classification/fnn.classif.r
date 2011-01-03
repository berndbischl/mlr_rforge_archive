#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.classif.r
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
    
    desc = c(
      oneclass = FALSE,
      twoclass = TRUE,
      multiclass = TRUE,
      missings = FALSE,
      doubles = TRUE,
      factors = FALSE,
      prob = FALSE,
      decision = FALSE,
      weights = FALSE,
      costs = FALSE
    )
    
    # l is for reject option. cannot be done with mlr atm
    par.descs = list(
      new("par.desc.double", par.name="k", default=1L, lower=1L),
      new("par.desc.log", par.name="use.all", default=TRUE, requires=expression(algorithm == "VR")),
      new("par.desc.disc", par.name="algorithm", default="cover_tree", vals=list("cover_tree", "kd_tree", "VR"))
    )
    callNextMethod(.Object, pack="FNN", desc=desc, par.descs=par.descs)
  }
)

#' @rdname train.learner

setMethod(
  f = "train.learner",
  signature = signature(
    .learner="classif.fnn", 
    .task="classif.task", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    y = .task["targets"][.subset]
    train = get.data(.task, .subset, with.target=FALSE)
    list(train=train, y=y, parset=list(...))
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "classif.fnn", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    m = .model["learner.model"]
    pars = list(train=m$train, test=.newdata, cl=m$y)  
    pars = c(pars, m$parset, list(...))
    p = do.call(FNN::knn, pars)
    attr(p, "nn.index") = NULL
    attr(p, "nn.dist") = NULL
    return(p)
  }
)	




