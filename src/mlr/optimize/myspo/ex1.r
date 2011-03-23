set.seed(1)
#source("D:\\sync\\projekte\\mlr\\src\\base\\optimize\\myspo\\ex0.r")
#
#f1 = function(x) sin(x/2)* (-x^2 + 400)* ifelse(abs(x) > 20, exp(-x^2) , 1)
#f2 = function(x) f1(x) + rnorm(1, 0, 30)
#
#par.set = list(      
#  new("Parameter.double", par.name="x", lower=-25, upper=10)
#)
#
##ml = makeLearner("regr.km.noisy")
#ml = makeLearner("regr.km")
#ctrl = myspo.optcontrol(par.set=par.set, meta.learner=ml, 
#  seq.method="DiceOptim.CL", seq.loops=20, init.des.points=15)
#z = myspo(f2, control=ctrl) 

x = seq(-30,30, 0.1)
y1 = sapply(x, f1)
y2 = sapply(x, f2)
p = predict(z$m@learner.model, newdata=matrix(x, ncol=1), type="UK")
g = data.frame(x=x, y1=y1, y2=y2)

plot(g$x, g$y1, type="l")
i = 1:ctrl$init.des.points
points(z$path$x[i], z$path$y[i], col="blue")
lines(g$x, p$mean, col="red", lty="dotted")
lines(g$x, p$lower95, col=rgb(0.4,0,0,alpha=0.3))
lines(g$x, p$upper95, col=rgb(0.4,0,0,alpha=0.3))
abline(v=z$opt[[1]])

#polygon(c(x, rev(x)), c(y1, rev(y2)), col="red")

#mypanel <- function(...) { 
#  panel.xyplot(...)
#  #panel.xyplot(g$x,g$y2, data=g, type="l")
#  panel.xyplot(z$path$x, z$path$y) 
#}
#plot = xyplot(y1 ~ x, data=g, type="l", panel=mypanel)
#print(plot)
#
#f1 <- function(x) sin(x)
#f2 <- function(x) cos(x)
#x <- seq(0, 2*pi, length.out=50)
#y1 <- f1(x)
#y2 <- f2(x)
#plot(x, y1, type="l")
#lines(x, y2)
#polygon(c(x, rev(x)), c(y1, rev(y2)), col="red")

