library(brew)
source("defs.R")
source("getResampling.R")

#todo: only use binary measures for binary tasks
# save training and predict time
# how to choose time for larger tasks?
# create tuned svm
# have a tuned version for every learner later on
# how to keep the queue filled?

# number of jobs =
# 200 ds x 10 learners x 5 splits = 10000
# but we dont have all ds already....
# seems doable in a few days


submitJob = function(ds, learner, split, split.name, measure) {
  fn.out = makeFileName(jobs.path, "job", ds, learner, split.name, measure, "R")
  e = new.env()
  e$ds = ds
  e$learner.id = learner@id
  e$split = split
  e$split.name = split.name
  e$measure.id = measure@id
  brew("job_template.R", fn.out, envir = e)
  wt = 1000L
  s = sprintf("~olafm/bin/submitR %s walltime=%i outdir=submitR", fn.out, wt)
  message("Submitting: ", s)
  system(s)
}

doNextExps = function(n) {
  count = 1L
  for (ds in dss.classif) {
    for (learner in learners.classif) {
      for (i in 1:length(rin.splits)) {
        split = rin.splits[i]
        split.name = names(rin.splits)[i]
        for (ms in measures) {
          fn = makeFileName(results.path, "result", ds, learner, split.name, ms, "RData")
          if (!file.exists(fn)) {
            if (is(learner, "TuneWrapper") || ms@id == measures[[1]]@id) {
              if (count > n)  
                return()
              # to this here so the master creates the resampling instance
              rin = getResampling(ds, split, split.name)
              submitJob(ds, learner, split, split.name, ms)
              count = count + 1L  
            }
          }
        }
      }
    }
  }  
}

args = commandArgs(TRUE)
n = as.integer(args[1])
doNextExps(n)