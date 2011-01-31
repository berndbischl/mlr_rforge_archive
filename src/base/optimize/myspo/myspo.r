
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
#


#todo: set seed to make reproducible
myspo = function(fun, par.set, des, learner, control, opt.path) {
  if (length(opt.path@y.names) > 1)
    stop("'opt.path' should only contain one 'y' column!")
  y.name = opt.path@y.names
  for (i in 1:nrow(des))
    add.path.el(opt.path, x=as.list(des[i,colnames(des)!=y.name]), y=des[i,y.name])
  rt = make.task(target=y.name, data=des)
  model = train(learner, rt)
  loop = 1
  while(loop <= control$seq.loops) {
    xs = proposePoints(model, par.set, control)
    y = sapply(xs, fun)
    Map(function(x, y1) add.path.el(opt.path, x=x, y=y1), xs, y)
    rt = make.task(target=y.name, data = as.data.frame(opt.path), exclude=c(".dob", ".eol"))
    model = train(learner, rt)
    loop = loop + 1  
  }
}






