generateVarsel2dResult <- function(learner, task, resampling, measures, control, pairs, remove) {
  if (is.character(learner))
    learner <- makeLearner(learner)
  if (is(resampling, "ResampleDesc") && control@same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
  if (missing(measures))
    measures = mlr:::default.measures(task)
  if (is(measures, "Measure"))
    measures = list(measures)   
  if (!(is(control, "VarselControlSequential") && control@method == "sfs"))
    stop("'control' must be VarselControlSequential with method 'sfs'!")
  # todo: document this! really do this? maybe dont have the user pass the control...!
  control@alpha = -Inf
  control@max.vars = 2L  
  if (missing(pairs))
    pairs = 4L
  if (missing(remove))
    remove = 1L
  if (!(remove == 1 || remove == 2))
    stop("'remove' must be 1 or 2!")
  
  ors = list()
  errs = matrix(TRUE, nrow=task@desc@size, pairs)
  mode(errs) = "logical"
  features = getFeatureNames(task)
  for (i in 1:pairs) {
    if (length(features) < 2)
      stop("Not enough features left to find another pair!")
    ors[[i]] = varsel(learner, task, resampling, control, measures)
    task2d = subsetData(task, vars=ors[[i]]@x)
    r = resample(learner, task2d, resampling)
    e = r$pred@df$truth != r$pred@df$response
    errs[,i] = e[order(r$pred["id"])]
    # get first selected feature
    pp = as.data.frame(ors[[i]]@path)
    pp = subset(pp, eol!=dob, select=features)
    best = colnames(pp)[pp[2,] == 1]
    features = setdiff(features, best)
    task = subsetData(task, vars=features)
  } 
  return(list(ors=ors, errs=errs))
}
