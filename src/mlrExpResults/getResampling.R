getResampling = function(ds, split, split.name) {
  fn = sprintf("subsample_%s_%s.RData", ds, split.name)
  fn = file.path(rin.path, fn)
  if (file.exists(fn)) {
    message("Loading: ", fn)
    load(fn)
  } else {
    message("Creating: ", fn)
    e = getDataset(ds, attach=FALSE)
    rin = makeResampleInstance(
      makeResampleDesc("Subsample", iters=rin.iters, split=split), 
      task=e$task
    )
    save(rin, file=fn)
  }
  return(rin)  
}