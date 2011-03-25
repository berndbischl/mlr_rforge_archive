source("../skel/R/parameter_space.R")
source("../skel/R/paramils.R")

f_noisy_sphere <- function(...) {
  x <- c(...)
  crossprod(x) + rnorm(1, 0, 1)
}

random_search <- function(f, par, control) {
  curpar <- random_parameter(par)
  curval <- do.call(f, curpar)
  n <- 1
  while(n < control$maxeval) {
    newpar <- random_parameter(par)
    newval <- do.call(f, newpar)
    n <- n + 1
    if (newval < curval) {
      curpar <- newpar
      curval <- newval
    }
  }
  list(par=curpar, value=curval)
}

random_search2 <- function(f, par, control) {
	curpar <- random_parameter(par)
	curval <- do.call(f, curpar)
	n <- 1
	while(n < control$maxeval) {
		newpar = random_neighbour(curpar)
		newval <- do.call(f, newpar)
		n <- n + 1
		if (newval < curval) {
			curpar <- newpar
			curval <- newval
		}
	}
	list(par=curpar, value=curval)
}


x <- seq(-5, 5, length.out=11)
pars <- discrete_parameter_space(x01=x, x02=x, x03=x, x04=x, x05=x,
                                 x06=x, x07=x, x08=x, x09=x, x10=x,
                                 x11=x, x12=x, x13=x, x14=x, x15=x
                                 )

control <- list(maxeval=1000L)

sim <- function(f) {
 # res <- parameter_ils(random_parameter(pars), f, pars, control)
  res <- parameter_ils(random_parameter(pars), f, pars)
  d1 <- sqrt(crossprod(unlist(res$par)))
  rres <- random_search(f, pars, control)
  d2 <- sqrt(crossprod(unlist(rres$par)))
  if (d1 < d2)
    cat("+")
  else
    cat("-")
  flush.console()
  d1 < d2
}

res <- replicate(50, sim(f_noisy_sphere))
print(prop.test(table(res), alternative="less"))
