#todo test
#' @export
makeDesign = function(n, par.set, fun=randomLHS, fun.args=list(), trafo=TRUE) {
  require.packs("lhs", "makeDesign")
  
  lower = lower(par.set)
  upper = upper(par.set)
  vals = values(par.set)
  pars = par.set@pars
  
  k = sum(sapply(par.set, function(x) if (x@type %in% c("numericvector", "integervector")) length(lower(x)) else 1))
  des = do.call(fun, c(list(n=n, k=length(pars)), fun.args))
  
  des = as.data.frame(des)
  
  col = 0
  for (i in 1:length(pars)) {
    p = pars[[i]]
    if (p@type %in% c("numericvector", "integervector")) 
      col = (col[length(col)] + 1) : (col[length(col)] + length(lower(p)))   
    else 
      col = (col[length(col)] + 1) : (col[length(col)] + length(lower(p)))   
    if (p@type == "numeric")
      des[,col] = p@trafo((upper(p)-lower(p))*des[,col] + lower(p))
    else if (p@type == "integer")
      des[,col] = p@trafo(floor((upper(p)-lower(p)+1)*des[,col] + lower(p)))
    else if (p@type == "numericvector") {
      des[,col] = t((upper(p)-lower(p))*t(des[,col]) + lower(p))
      des[,col] = apply(des[,col], 1, p@trafo)
    } else if (p@type == "integervector")
      des[,col] = p@trafo((upper(p)-lower(p))*des[,col] + lower(p))
    else if (p@type == "logical")
      des[,col] = ifelse(des[,col] <= 0.5, FALSE, TRUE)
    else if (p@type == "discrete") {
      v = values(p)
      des[,col] = factor(names(v[ceiling(des[,col] * length(v))]), levels=v)
    }
  }
  colnames(des) = sapply(pars, function(x) x@id)
  return(des)
}
