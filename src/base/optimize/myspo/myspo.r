#todo: learn 2nd meta model for constraint violation, so we only
# generate reasonable points in seqdes?  we need confidence ingtervals
# in prediction! then we can use EI
myspo = function(fun, control) {
  # todo req.packs
  require(lhs)
  pds = control$par.descs
  control$constr.learner = make.learner("classif.randomForest")
  ml = control$meta.learner
  cl = control$constr.learner  
  
  curdes = init.design(pds, control$init.des.points)
  cury = eval.des.with.fun(curdes, fun, control)
  print(cbind(curdes, cury))
  tmm = train.meta.model(ml, cl, curdes, cury, control)
  loop = 1  
  while(loop <= control$seq.loops) {
    print(loop)
    seqdes = seq.design(pds, control$seq.des.points, tmm$constr.model)
    ## print(str(seqdes))
    newdes = choose.new.points(1, tmm$meta.model, tmm$constr.model, curdes, cury, control)
    ## newdes = rbind(newdes, unlist(sel.random(pds, "par.desc.num")))
    ## newy = fun(newdes, control)
    newy = eval.des.with.fun(newdes, fun, control)
    ## print(cbind(newdes, newy))
    curdes = rbind(curdes, newdes)
    cury = c(cury, newy)
    tmm = train.meta.model(ml, cl, curdes, cury, control)
    loop = loop + 1    
  }
  fp = choose.final.point(tmm$meta.model, tmm$constrmodel, fun, curdes, cury, control)
  curdes[, control$y.name] = cury
  list(opt=fp$x, y.meta=fp$y.meta, y.real=fp$y.real, y.diff=fp$y.diff, path=curdes, 
    meta.model=tmm$meta.model, constr.model=tmm$constr.model)  
}

f <- function(x, ...) {
  stopifnot(is.list(x))
  
}

spo_function <- function(f) {
  function(x, ...) {
    f(unlist(x), ...)
  }
}

myspo(..., spo_function(f), ...)

myspo <- function(par, fun, bounds, learner, control) {
  
  while(loop <= control$seq.loops) {
    xs = propose.points(n, model, control)
    ys = unlist(mylapply(xs, fun))
    op = add.els(op, xs, ys)
    rt = make.task(opt.path)
    model = train(learner, rt)
    loop = loop + 1    
  }
}


make.tas


myspo(lhs_from_bounds(bounds, 200), fun, bounds, model, control)

opt.path: s4object, liste von listen
ableiten für extra sachen
enthält pro iteratuin: x werte, y werte (wenn mehr), iteration 
as.data.frame, print, subset, plot


#' @export
setMethod(
  f = "make.task",
  
  signature = signature(
    id="character", 
    data="opt.path", 
    target="missing", 
    exclude="missing", 
    weights="missing", 
    blocking="missing",
    control="missing",
    costs="missing",
    positive="missing"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, control, costs, positive) {
    
    df = as.data.frame(data)
    #subset only to xs and first y
    df[]
    make.task(data=df, target=<first y name>)
  }
)




