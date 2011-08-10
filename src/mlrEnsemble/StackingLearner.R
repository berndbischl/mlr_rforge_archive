#“On Cross-Validation and Stacking: Building Seemingly Predictive Models on Random Data”
#Claudia Perlich, Grzegorz Swirszcz. SIGKDD Explorations 12(2) (2010) 11-15
#Is Combining Classiers Better than Selecting the Best One?



#todo: stacking for classif

#' @exportClass StackingLearner
setClass(
  "StackingLearner",   
  contains = c("BaseCombiner"),
  representation = representation(
    super.learner = "Learner",
    resampling = "ResampleDesc"
  )
)   


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("StackingLearner"),
  def = function(.Object, base.learners, super.learner, resampling) {
    .Object@super.learner = super.learner
    .Object@resampling = resampling
    callNextMethod(.Object, learners=base.learners, par.set=makeParameterSet())
  }
)

setGeneric(
  name = "makeStackingLearner",
  def = function(id, base.learners, super.learner, resampling) {
    if(missing(id))
      id = "Stacking"
    standardGeneric("makeStackingLearner")
  }
)


#' @rdname makeStackingLearner
setMethod(
  f = "makeStackingLearner",
  
  signature = signature(
    id="character", 
    base.learners="list", 
    super.learner="Learner", 
    resampling="ResampleDesc" 
  ),
  
  def =  function(id, base.learners, super.learner, resampling) {
    #todo: check that base.learners and super.learner types match
    mlr:::check.list.type(base.learners, "Learner")
    types = unique(sapply(base.learners, function(x) x@properties$type))
    if (length(types) != 1)
      stop("Base learners must all have the same type ('classif' or 'regr')!")
    if (types != super.learner@properties$type)
      stop("Base learner type must match super learner type!")
    if (types != "regr")
      stop("Stacking currently only implemented for regression!")
    if (!is(resampling, "CVDesc")) 
      stop("Currently only CV is allowed for resampling!")
    w = new("StackingLearner", base.learners, super.learner, resampling)
    w@properties$type = types
    w@properties["numerics"] = all(sapply(base.learners, function(x) x@properties$numerics))
    w@properties["factors"] = all(sapply(base.learners, function(x) x@properties$factors))
    w@properties["weights"] = all(sapply(base.learners, function(x) x@properties$weights))
    w@properties["missings"] = all(sapply(base.learners, function(x) x@properties$missings))
    w@properties[["oneclass"]] = FALSE
    w@properties[["twoclass"]] = FALSE
    w@properties[["multiclass"]] = FALSE
    w@properties[["prob"]] = FALSE
    w@properties[["decision"]] = FALSE
    w@properties[["costs"]] = FALSE
    w@id = id
    return(w)
  }
)

#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="StackingLearner", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    tn = .task@desc@target
    learners = .learner@learners 
    ids = sapply(learners, function(x) x@id)
    if (tn %in% ids) 
      stop("Learner id is equal to target name of task!")
    .task = subset(.task, subset=.subset)  
    rin = makeResampleInstance(.learner@resampling, .task)
    inds = do.call(c, rin@test.inds) 
    resps = lapply(learners, function(w) resample(w, .task, rin)$pred@df$response)
    resps = as.data.frame(do.call(cbind, resps))
    colnames(resps) = ids
    resps[[tn]] = getTargets(.task)[inds]
    super.task = makeRegrTask(data=resps, target=tn)
    super.model = train(.learner@super.learner, super.task)
    base.models = lapply(.learner@learners, function(w) train(w, .task))
    list(base.models=base.models, super.model=super.model)
  }
)



#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "StackingLearner", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    z = .model@learner.model
    resps = lapply(z$base.models, function(m) predict(m, newdata=.newdata)@df$response)
    resps = as.data.frame(do.call(cbind, resps))
    #0.5*resps[,1] + 0.5*resps[,2] - 1 
    colnames(resps) = sapply(.learner@learners, function(x) x@id)
    predict(z$super.model, newdata=resps)@df$response
  }
)   

#todo: print method

