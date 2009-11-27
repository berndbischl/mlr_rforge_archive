ps.wrapper <- function(f, start, lower, upper, control) {
	if (is.null(control))
		ps <- gp.search(f=f, start=start, lower=lower, upper=upper)
	else
		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper, control=control)
	par <- as.list(ps$par)
	list(par=par, val=ps$val, path=ps$path)
}




gp.search <- function(f, lower=lower, upper=upper, n=length(lower), start.vals) {
	bounds = cbind(lower, upper)
	x = apply(bounds, 1, function(a) runif(start.vals, a[1], a[2]))
	y = apply(x, 1, f)
	
	mydf = as.data.frame(x)
	mydf$y=y
	print(mydf)
	x.new = c(0,0)
	delta = 100
	for (k in 1:100) {
		gp = gausspr(mydf[,1:n], mydf$y, kpar=list(sigma=1))
		gp.f <- function(a) {
			a = matrix(a, nrow=1)
			predict(gp, a)
		}
		#j.start = sample(1:nrow(df),1)
		#start = as.numeric(df[j.start, 1:n])
		#or = cma_es(fn=gp.f, par=start, control=list(maxit=200))
		#x.new = or$par
		#y.new = f(x.new)
		#print(x.new)
		x.test = expand.grid(
				seq(x.new[1] - delta, x.new[1] + delta, length.out=100),
				seq(x.new[2] - delta, x.new[2] + delta, length.out=100)
		)
		delta = delta * 0.9
#		print(x.test)
		y.test =predict(gp, x.test)
#		print(y.test)
		j = which.min(y.test) 
#		print(j)
		x.new = as.numeric(x.test[j, , drop=T])
		y.new = f(x.new)
		z.new = c(x.new, y.new)
#		browser()
#		print(z.new)
		mydf <- rbind(mydf, z.new)
		mm <<- mydf
		print(mydf[nrow(mydf),])
	}
}
library(kernlab)
or = gp.search(f=function(x) sum(x^2), lower=c(-100,-100), upper=c(100,100), start.vals=15)


#gp=gausspr(mm[,1:2], mm$y, kpar=list(sigma=1))
#y.test =predict(gp, x.test)
#contour(seq(-100,100,0.5), seq(-100,100,0.5), matrix(y.test, 401), nlevels=50)
