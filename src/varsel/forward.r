

sel.control <- function(compare="diff", eps=0.01, steps=Inf) {
	list(compare=compare, eps=eps, steps=steps)
}

forward.sel <- function(learn.task, resample.desc, ranges=list(), control=sel.control()) {
	is <- learn.task["input.names"]
	rin <- make.resample.instance(size=nrow(learn.task@data), desc=resample.desc)
	state <- eval.state(learn.task, rin, vars=character(0))
			
	while (TRUE) {
		logger.debug("current:")
		logger.debug(state$vars)
		logger.debug(state$perf$aggr)
		not.used <- setdiff(is, state$vars)
		rin <- make.resample.instance(size=nrow(learn.task@data), desc=resample.desc)
		best.kid.state <- NA 
		for (v in not.used) {
			print(v)
			new.vars <- c(state$vars, v)
			new.state <- eval.state(learn.task, rin, vars=new.vars)
			if (is.na(best.kid.state) || best.kid.state$perf$aggr > new.state$perf$aggr) {
				best.kid.state <- new.state
			}
		}
		
		if (compare == "diff") {
			m1 <- mean(state$perf$values)
			m2 <- mean(best.kid.state$perf$values)
			d <- m1 - m2
			if (d > eps)
				state <- best.kid.state
			else 
				break
		}
		return(state)
#		alpha <- 0.15
#		test <- wilcox.test(state$perf$values, 
#				best.kid.state$perf$values, 
#				alternative="greater",
#				alpha=alpha)
#		print(test)
#		if (test$p.value < alpha) {
#			state <- best.kid.state
#		} else {
#			break
#		}
		#		if (test$p.value < alpha) {
#			state <- best.kid.state
#		} else {
#			break
#		}
		
	}
	return(state)
}

backward.sel <- function(learn.task, steps=1, resample.desc, ranges=list(), eps=0.001) {
	is <- learn.task["input.names"]
	rin <- make.resample.instance(size=nrow(learn.task@data), desc=resample.desc)
	state <- eval.state(learn.task, rin, vars=is)

	while (TRUE) {
		print("current:")
		print(state)
		rin <- make.resample.instance(size=nrow(learn.task@data), desc=resample.desc)
		best.kid.state <- NA 
		for (v in state$vars) {
			new.vars <- setdiff(state$vars, v)
			new.state <- eval.state(learn.task, rin, vars=new.vars)
			if (is.na(best.kid.state) || best.kid.state$perf > new.state$perf) {
				best.kid.state <- new.state
			}
		}
		print(best.kid.state)
		if (best.kid.state$perf - state$perf < eps) {
			state <- best.kid.state
		} else {
			break
		}
	}
	return(state)
}


eval.state <- function(learn.task, resample.instance, vars) {
	rf <- resample.fit(learn.task, resample.instance, vars=vars)
	rp <- resample.performance(learn.task, resample.instance, rf)
	return(list(vars=vars, perf=rp))
}


