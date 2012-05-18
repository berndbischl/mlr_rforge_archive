
makeResampleInstanceHoldout = function(desc, size, task) {
  makeResampleInstance(desc=desc, size=size)
}


makeResampleInstanceCV = function(desc, size, task) {
  test.inds = sample(1:size)
  # don't warn when we can't split evenly
  test.inds = suppressWarnings(split(test.inds, 1:desc@iters))
  makeResampleInstance(desc=desc, size=size, test.inds=test.inds)
  makeResampleInstance(desc=desc, size=size)
}


makeResampleInstanceLOO = function(desc, size, task) {
  desc@iters = size
  makeResampleInstance(desc=desc, size=size, test.inds=as.list(1:size))
  makeResampleInstance(desc=desc, size=size)
}

makeResampleInstanceLOO = function(desc, size, task) {
  inds = boot(1:size, R=desc@iters, function(data,inds) inds)$t
  inds = as.list(as.data.frame(t(inds)))
  names(inds) = NULL
  makeResampleInstance(desc=desc, size=size, train.inds=inds)
  makeResampleInstance(desc=desc, size=size, test.inds=as.list(1:size))
  makeResampleInstance(desc=desc, size=size)
}

makeResampleInstanceSubsample = function(desc, size, task) {
  inds = lapply(1:desc@iters, function(x) sample(1:size, size*desc@split))
  makeResampleInstance(desc=desc, size=size, train.inds=inds)
}



setMethod(
  f = "initialize",
  signature = signature("RepCVInstance"),
  def = function(.Object, desc, size, task) {
    folds = desc@iters/desc@reps
    d = makeResampleDesc("CV", iters=folds)
    i = replicate(desc@reps, makeResampleInstance(d, size=size), simplify=FALSE)
    train.inds = Reduce(c, lapply(i, function(j) j@train.inds))
    test.inds = Reduce(c, lapply(i, function(j) j@test.inds))
    g = as.factor(rep(1:desc@reps, each=folds))
    callNextMethod(.Object, desc=desc, size=size, train.inds=train.inds, test.inds=test.inds, group=g)
  }
)


#setMethod(
#  f = "initialize",
#  signature = signature("StratCVInstance"),
#  def = function(.Object, desc, size, task) {
#    if (is.null(task))
#      stop("stratcv always needs to be passed the task, otherwise stratification is impossible!")
#    if (task@desc@type != "classif")
#      stop("stratcv is currently only supported for classification!")
#    y = getTargets(task)
#    k = desc@iters
#    # CV on every class
#    class.inds = lapply(task@desc@class.levels, function(x) which(x==y))
#    test.inds = lapply(class.inds, function(x) suppressWarnings(split(sample(x), 1:k)))
#    # combine them all, so we have the test.inds
#    test.inds = Reduce(function(i1, i2) Map(c, i1, i2), test.inds)
#    callNextMethod(.Object, desc=desc, size=size, test.inds=test.inds)
#  }
#)
#

