#' @S3method makeRLearner regr.randomForest
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="ntree", default=500L, lower=1L),
      # FIXME: think about the following two params
      makeIntegerLearnerParam(id="ntree_for_sd", default=100L, lower=1L),
      makeDiscreteLearnerParam(id="se_method", default="bootstrap", values=c("bootstrap", "jackknife", "noisy.bootstrap")),
      makeIntegerLearnerParam(id="mtry", lower=1L),
      makeLogicalLearnerParam(id="replace", default=TRUE),
      makeIntegerLearnerParam(id="sampsize", lower=1L),
      makeIntegerLearnerParam(id="nodesize", default=1L, lower=1L),
      makeIntegerLearnerParam(id="maxnodes", lower=1L),
      makeLogicalLearnerParam(id="importance", default=FALSE),
      makeLogicalLearnerParam(id="localImp", default=FALSE),
      makeLogicalLearnerParam(id="keep.inbag", default=FALSE)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = TRUE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.randomForest
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights, ...) {
  f = getTaskFormula(.task)

  m = randomForest(f, data=getTaskData(.task, .subset), ...)

  # we have to do some preprocessing here if we need the standard error
  if (.learner$predict.type == "se") {
    par.vals = list(...)
    train = getTaskData(.task, .subset)
    models = list()

    if (par.vals$se_method %in% c("bootstrap", "noisy.bootstrap")) {
      # set some params for bootstraping
      # FIXME: make parameters out of this 
      numberOfBootstraps = 5
      bootstrapSize = nrow(train)

      # generate bootstrap samples
      samplesIdx = replicate(numberOfBootstraps, sample(1:bootstrapSize, replace=TRUE))

      # determine whether we work with reduced ensemble size (noisy bootstrap) or not
      ntree = if (par.vals$se_method == "bootstrap") par.vals$ntree else par.vals$ntree_for_sd

      print("Fitting bootstrap models.")
      # fit models on the bootstrap samples
      models = apply(samplesIdx, 2, function(bootstrapIdx) {
        randomForest(f, data=train[bootstrapIdx,], keep.forest=TRUE, ...)
      })
    } else {
      # prepare models for jackknife method
      for (i in 1:nrow(train)) {
        # necessarily keep forest and keep track which samples are in-bag
        models = randomForest(f, data=train[-i,], keep.forest=TRUE, keep.inbag=TRUE, ...)
      }
    }
    # save models in attrribute
    attr(m, "mlr.se.bootstrap.models") = models
  }
  return(m)
}

#' @S3method predictLearner regr.randomForest
predictLearner.regr.randomForest = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "se") {
    par.vals = .learner$par.vals
    print("Computing standard error...")
    model = .model$learner.model
    print(model)
    seFun = getSEFun(par.vals$se_method)
    seFun(.learner, .model, .newdata, ...)
  } else {
    predict(.model$learner.model, newdata=.newdata, ...)
  }
}

getSEFun = function(method) {
  supportedSEEstimators = getSupportedSEEstimators()
  checkArg(method, "character", len=1L, choices=names(supportedSEEstimators))
  return (supportedSEEstimators[[method]])
}

getSupportedSEEstimators = function() {
  list("bootstrap" = bootstrapStandardError,
       "noisy.bootstrap" = bootstrapStandardError,
       "jackknife" = jackknifeStandardError)
}

bootstrapStandardError = function(.learner, .model, .newdata, ...) {
    # copy learner and change response type
    print("Entering bootstrap SE method")

    par.vals = .learner$par.vals
    learner = .learner
    learner = setPredictType(learner, "response")

    #stop("implementing...")
    models = attr(model$learner.model, "mlr.se.bootstrap.models")
    B = length(models)
    R = par.vals$ntree
    M = par.vals$ntree_for_sd
    catf("Number of bootstrap samples: %i", B)
    catf("Reduced number of ensembles: %i", R)
    catf("Number of ensembles        : %i", M)

    # make predictions for newdata based on each "bootstrap model"
    preds = lapply(models, function(model) {
      # save predictions of every single ensemble member, i.e., decision tree
      predict(model, .newdata, predict.all=TRUE)
    })
    
    aggr_responses = as.data.frame(lapply(preds, function(p) p$aggregate))
    names(aggr_responses) = 1:B

    print(aggr_responses)
    mean_responses = rowMeans(aggr_responses)
    cat("MEAN responses:")
    print(mean_responses)

    ind_responses = lapply(preds, function(p) p$individual)
    names(ind_responses) = NULL
    #print(ind_responses)

    # compute (brute-force) bootstrap standard error
    res = matrix(NA, ncol=2, nrow=nrow(.newdata))
    for (i in 1:nrow(.newdata)) {
      res[i,] = c(mean_responses[i], sum((aggr_responses[i,] - mean_responses[i])^2)/(B-1))
    }

    if (par.vals$se_method == "noisy.bootstrap") {
      print("CORRECTING BIAS...")
      # Bias contributed significantly to the error of the biased bootstrap estimator
      # Thus, compute a corrected version
      # FIXME: check if this works properly
      for (i in 1:nrow(.newdata)) {
        bias = 0
        for (b in 1:B) {
          for (r in 1:R) {
            bias = bias + (ind_responses[[b]][i,r] - aggr_responses[i,b])^2
          }
        }
        # FIXME: factor missing
        bias = bias * ((1/R) - (1/M))/(B*R*(R-1))
        # catf("BIAS is %f", bias)
        res[i,2] = res[i,2] - bias
      }
    }

    res[,2] = sqrt(res[,2])

    print(res)
}

jackknifeStandardError = function(.learner, .model, .newdata) {
    print("Entering jackknife SE method")
    stop("NOT IMPLEMENTED YET")
}