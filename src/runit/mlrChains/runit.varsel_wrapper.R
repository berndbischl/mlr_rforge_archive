test.varselWrapper <- function() {
  inner = makeResampleDesc("CV", iter=2)
  ctrl = makeVarselControlSequential(method="sfs", alpha=10)
  wl = makeVarselWrapper("classif.lda", resampling=inner, control=ctrl)
  outer = makeResampleDesc("CV", iter=2)
  r = resample(wl, task=multiclass.task, resampling=outer)
}

