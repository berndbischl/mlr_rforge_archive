seq.design = function(par.set, n.points, constr.model) {
  des = as.data.frame(matrix(0, 0, 0))
  while(nrow(des) < n.points) {
    des2 = as.data.frame(matrix(0, n.points, 0))
    for (i in seq(length=length(par.set))) {
      pd = par.set[[i]]
      des2[[pd["par.name"]]] = sample.pardesc(n.points, pd)
    }
    if (!is.null(constr.model)) {
      p = predict(constr.model, newdata=des2)
      good = as.logical(p["response"])
      des = rbind(des, des2[good,])
    } else {
      des = des2
    }
  }
  return(des)
}

