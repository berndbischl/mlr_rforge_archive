
report = function(data, target, exclude=character(0)) {
  cns = colnames(data)
  print(summarizeData(data))
  
  cols = which(!(cns %in% exclude))
  ps = lapply(cols, function(i) plotFeatureDistrib(data[,i], cns[i], data, target))
  do.call(grid.arrange, ps)
  pause()
  plotPCA(data, target)
  pause()
  plotMDS(data, target)
  pause()
}

