#if (length(losses) > 0) {
#  ls = lapply(losses, function(f) f(x, task=task))
#  ls = as.data.frame(Reduce(cbind, ls))
#  colnames(ls) = names(losses)
#  if (!is.null(x["id"]))
#    ls = cbind(id=x["id"], ls)
#}
#
#if (length(losses) > 0)
#  return(list(measures=ms, losses=ls))
