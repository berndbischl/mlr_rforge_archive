
add.path.varsel = function(path, es, accept) {
	add.path(path, es, accept)
} 

add.path.els.varsel = function(path, ess, best) {
	add.path.els(path, ess, best)	
} 

vars.to.logical = function(vars, all.vars) {
  if (is.list(vars)) {
    y = t(sapply(vars, function(x) all.vars %in% x))
    colnames(y) = all.vars
  } else {
    y = all.vars %in% vars
    names(y) = all.vars
  }
  y
}

vars.to.binary = function(vars, all.vars) {
  y=vars.to.logical(vars, all.vars)
  mode(y) = "integer"
  y
}

logical.to.vars = function(x, all.vars) {
  if (is.matrix(x)) {
    if (missing(all.vars))
      all.vars = colnames(x)
    lapply(1:nrow(x), function(i) all.vars[x[i,]])
  } else {
    if (missing(all.vars))
      all.vars = names(x)
    all.vars[x]
  }
}

binary.to.vars = function(x, all.vars) {
  mode(x) = "logical"
  logical.to.vars(x, all.vars)  
}


