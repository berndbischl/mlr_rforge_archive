##todo: learn 2nd meta model for constraint violation, so we only
## generate reasonable points in seqdes?  we need confidence ingtervals
## in prediction! then we can use EI
#myspo = function(fun, control) {
#  # todo req.packs
#  require(lhs)
#  pds = control$par.set
#  control$constr.learner = make.learner("classif.randomForest")
#  ml = control$meta.learner
#  cl = control$constr.learner  
#  
#  curdes = init.design(pds, control$init.des.points)
#  cury = eval.des.with.fun(curdes, fun, control)
#  print(cbind(curdes, cury))
#  tmm = train.meta.model(ml, cl, curdes, cury, control)
#  loop = 1  
#  while(loop <= control$seq.loops) {
#    print(loop)
#    seqdes = seq.design(pds, control$seq.des.points, tmm$constr.model)
#    ## print(str(seqdes))
#    newdes = choose.new.points(1, tmm$meta.model, tmm$constr.model, curdes, cury, control)
#    ## newdes = rbind(newdes, unlist(sel.random(pds, "Parameter.num")))
#    ## newy = fun(newdes, control)
#    newy = eval.des.with.fun(newdes, fun, control)
#    ## print(cbind(newdes, newy))
#    curdes = rbind(curdes, newdes)
#    cury = c(cury, newy)
#    tmm = train.meta.model(ml, cl, curdes, cury, control)
#    loop = loop + 1    
#  }
#  fp = choose.final.point(tmm$meta.model, tmm$constrmodel, fun, curdes, cury, control)
#  curdes[, control$y.name] = cury
#  list(opt=fp$x, y.meta=fp$y.meta, y.real=fp$y.real, y.diff=fp$y.diff, path=curdes, 
#    meta.model=tmm$meta.model, constr.model=tmm$constr.model)  
#}
#
#f <- function(x, ...) {
#  stopifnot(is.list(x))
#  
#}
#
#spo_function <- function(f) {
#  function(x, ...) {
#    f(unlist(x), ...)
#  }
#}
#
#myspo(..., spo_function(f), ...)
#
#myspo <- function(par, fun, bounds, learner, control) {
#  
#  while(loop <= control$seq.loops) {
#    xs = propose.points(n, model, control)
#    ys = unlist(mylapply(xs, fun))
#    op = add.els(op, xs, ys)
#    rt = make.task(opt.path)
#    model = train(learner, rt)
#    loop = loop + 1    
#  }
#}
#
#
#make.tas
#
#
#myspo(lhs_from_bounds(bounds, 200), fun, bounds, model, control)
#
#opt.path: s4object, liste von listen
#ableiten für extra sachen
#enthält pro iteratuin: x werte, y werte (wenn mehr), iteration 
#as.data.frame, print, subset, plot
#
#
##' @export
#setMethod(
#  f = "make.task",
#  
#  signature = signature(
#    id="character", 
#    data="opt.path", 
#    target="missing", 
#    exclude="missing", 
#    weights="missing", 
#    blocking="missing",
#    control="missing",
#    costs="missing",
#    positive="missing"
#  ),
#  
#  def = function(id, data, target, exclude, weights, blocking, control, costs, positive) {
#    
#    df = as.data.frame(data)
#    #subset only to xs and first y
#    df[]
#    make.task(data=df, target=<first y name>)
#  }
#)
#
#
#

#des = init.design(control$init.des.points, bounds, control$init.des.fun, control$init.des.args)
#y = lapply(1:nrow(des), fun(as.list(function(i) des[i,])))
#des$.myspo.y = y

#todo: set seed to make reproducible
myspo = function(fun, bounds, des, learner, control, opt.path) {
  print(str(des))
  rt = make.task(target=opt.path@y.names, data=des)
  model = train(learner, rt)
  print(model)
  loop = 1
  while(loop <= control$seq.loops) {
    xs = propose.points(model, bounds, control)
    print(Reduce(rbind, lapply(xs, unlist)))
    y = sapply(xs, fun)
    print(str(y))
    Map(function(x, y1) add.path.el(opt.path, x=x, y=y1), xs, y)
    rt = make.task(opt.path)
    model = train(learner, rt)
    loop = loop + 1  
    stop(12)
  }
}

# returns list of points
propose.points = function(model, bounds, control) {
  if (control$propose.points.method == "seq.design") {
    des = make.design(control$seq.design.points, bounds, control$seq.design.fun, control$seq.design.args)
    print(str(des))
    y = predict(model, newdata=des)["response"]
    print(str(y))
    o = order(y)
    points = des[o[1:control$propose.points],]
    return(lapply(1:nrow(points), function(i) as.list(points[i,])))
  }
}

fun = function(x) {
  sum(unlist(x)^2)
}

bounds = make.bounds(
  makeNumericParameter("x1", lower=-10, upper=10),
  makeNumericParameter("x2", lower=-10, upper=10)
)

set.seed(1)
des = make.design(30, bounds, randomLHS, list())
y = sapply(1:nrow(des), function(i) fun(as.list(des[i,])))
des$.myspo_y = y
ctrl = list(seq.loops=3, propose.points=2, 
  propose.points.method="seq.design", seq.design.points=100, seq.design.fun=randomLHS, seq.design.args=list())
opt.path = new("opt.path", x.names=sapply(bounds@pars, function(p) p@id), y.names=".myspo_y")
myspo(fun, bounds, des, "regr.rpart", ctrl, opt.path)




