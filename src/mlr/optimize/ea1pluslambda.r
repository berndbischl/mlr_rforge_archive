ea = function(f, names.double, names.disc, lower, upper, vals, control) {
  asdf = function(pop) {
    Reduce(rbind, lapply(pop, as.data.frame))
  }
  
  init.pop = function() {
    pop = list()
    for (i in 1:control$mu) {
      x = list()
      for (v in names.double) {
        x[[v]] = runif(1, min=lower[v], max=upper[v])
        x[[paste(v,"sss")]] = runif(1, min=0, max=(upper[v]-lower[v])/10)
      }
      for (v in names.disc) 
        x[[v]] = sample(vals[[v]], 1)
      pop[[i]] = x
    }
    pop
  } 
  
  eval.pop = function(pop) {
    sapply(pop, function(x) {
        x[paste(names.double, "sss")]=NULL  
        do.call(f, x)
    })
  }
  
  select = function(pop) {
    pop[sample(1:length(pop), control$lambda, replace=TRUE)]
  }
  
  cross = function(x, y) {
    z = list()
    for (v in c(names.double, paste(names.double,"sss"))) 
      z[[v]] = mean(c(x[[v]], y[[v]]))
    for (v in names.disc) 
      z[[v]] = sample(c(x[[v]], y[[v]]), 1)
    z    
  }
  mutate = function(x) {
    for (v in paste(names.double, "sss")) 
      x[[v]] = x[[v]] * exp(rnorm(1,0,1))
    for (v in names.double) 
      x[[v]] = x[[v]] + rnorm(1,0,x[[paste(v, "sss")]])
      #x[[v]] = x[[v]] + rnorm(1,0,step.sizes[[v]]/100)
    for (v in names.disc) 
      if (runif(1) < control$alpha) x[[v]] = sample(vals[[v]], 1)
    x
  }
  
  step.sizes = sapply(names.double, function(v) (upper[v] - lower[v]) / 100, USE.NAMES=FALSE)
  
  pop = init.pop()
  print("init")
  print(asdf(pop))
  
  for (i in 1:control$gens) {
    parents1 = select(pop)
    #print("parents1")
    #print(asdf(parents1))
    parents2 = select(pop)
    #print("parents2")
    #print(asdf(parents2))
    kids = Map(cross, parents1, parents2)
    #print("kids")
    #print(asdf(kids))
    kids = lapply(kids, mutate)
    #print("mutated")
    #print(asdf(kids))
    pop = c(pop, kids)
    y = eval.pop(pop)
    # nextgen
    o = order(y)
    o = o[1:control$mu]
    pop = pop[o]
    print(mean(y[o]))
    #print(y)
    #print(asdf(pop))
  }
  return(pop=pop, y=y[o])
}

f = function(x,y,a,b) {
  (x+y)^2
}

#set.seed(1)
or = ea(
  f,
  names.double = c("x", "y"),
  names.disc = c("a", "b"), 
  lower = c(x=-100, y=-100),
  upper = c(x=10, y=10),
  vals =list(a=1:2, b=1:3),
  control = list(mu=6, lambda=10, alpha=0.5, gens=50)
)


