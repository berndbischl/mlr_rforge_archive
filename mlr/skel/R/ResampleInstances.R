instantiateResampleInstance = function(desc, size) {
  UseMethod("instantiateResampleInstance")
}

instantiateResampleInstance.HoldoutDesc = function(desc, size) {
  inds = sample(1:size, size*desc$split)
  makeResampleInstanceInternal(desc, size, train.inds=list(inds))
}


instantiateResampleInstance.CVDesc = function(desc, size) {
  test.inds = sample(1:size)
  # don't warn when we can't split evenly
  test.inds = suppressWarnings(split(test.inds, 1:desc$iters))
  makeResampleInstanceInternal(desc, size, test.inds=test.inds)
}


instantiateResampleInstance.LOODesc = function(desc, size) {
  desc$iters = size
  makeResampleInstanceInternal(desc, size, test.inds=as.list(1:size))
}

instantiateResampleInstance.SubsampleDesc = function(desc, size) {
  inds = lapply(1:desc$iters, function(x) sample(1:size, size*desc$split))
  makeResampleInstanceInternal(desc, size, train.inds=inds)
}

instantiateResampleInstance.BootstrapDesc = function(desc, size) {
  inds = boot(1:size, R=desc$iters, function(data,inds) inds)$t
  inds = as.list(as.data.frame(t(inds)))
  names(inds) = NULL
  makeResampleInstanceInternal(desc, size, train.inds=inds)
}

instantiateResampleInstance.RepCVDesc = function(desc, size) {
  folds = desc$iters / desc$reps
  d = makeResampleDesc("CV", iters=folds)
  i = replicate(desc$reps, makeResampleInstance(d, size=size), simplify=FALSE)
  train.inds = Reduce(c, lapply(i, function(j) j$train.inds))
  test.inds = Reduce(c, lapply(i, function(j) j$test.inds))
  g = as.factor(rep(1:desc$reps, each=folds))
  makeResampleInstanceInternal(desc, size, train.inds=train.inds, test.inds=test.inds, group=g)
}

