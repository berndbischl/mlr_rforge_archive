
varsel.hybrid = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	
	path = list()
	all.vars = task["input.names"]
	data = task["data"][,all.vars]
	cors = abs(cor(data, data))
	diag(cors) = NA
	m = length(all.vars) 
	p = 1 / m
	
	start = all.vars[as.logical(rbinom(m, 1, 0.5))]
	cat("start:", start, "\n")
	state <<- eval.state(learner, task, resampling, measures, aggr, vars=start)
	path[[1]] = state
	names(path)[1] = "start"
	print(get.perf(state, measures, aggr))
	# use alpha for mut
	ctrl2 = control
	ctrl2$alpha = 0
			
	
	# big loop for mut + local
	while (.mlr.vareval < control$maxit) {
		
		# mutate til successful
		vs.bin = all.vars %in% state$vars
		print(vs.bin)
		while (.mlr.vareval < control$maxit) {
			mut = as.logical(rbinom(m, 1, p))
			cat("mut:", mut, "\n")
			# xor
			new.bin = (vs.bin != mut)
			new.vars = all.vars[new.bin]
			#print(new.bin)
			cat("new.vars:", new.vars, "\n")
			new.state = eval.state(learner, task, resampling, measures, aggr, vars=new.vars)
			print(get.perf(new.state, measures, aggr))
			if (compare.diff(state, new.state, ctrl2, measures, aggr, forward=T) 
					&& length(new.state$vars) > 0) {
				print("accept")
				state=new.state
				path[[length(path)+1]] = state
				names(path)[length(path)] = "mut"
				break
			}
		}
		
		
		
		op = sample(c("plus", "minus"), 1)
		failed = c(plus=F, minus=F)
		while (.mlr.vareval < control$maxit) {
			# try minus or plus repeatedly
			found = F
			while (.mlr.vareval < control$maxit) {
				print(op)
				if (op == "minus") {
					cors2 = cors[state$vars, state$vars, drop=F]
					meancor = rowMeans(cors2, na.rm=T)
					cat("mc:", meancor, "\n")
					v = names(which.max(meancor))
					new.vars = setdiff(state$vars, v)
				} else {
					not.used = setdiff(all.vars, state$vars)
					cors2 = cors[not.used, state$vars, drop=F]
					meancor = rowMeans(cors2)
					cat("mc:", meancor, "\n")
					v = names(which.min(meancor))
					new.vars = c(state$vars, v)
				}
				cat("newvars:", new.vars, "\n")
				new.state = eval.state(learner, task, resampling, measures, aggr, vars=new.vars)
				print(get.perf(new.state, measures, aggr))
				if (compare.diff(state, new.state, control, measures, aggr, forward=(op=="plus"))) {
					print("accept")
					state=new.state
					path[[length(path)+1]] = state
					names(path)[length(path)] = op
					found = T
				} else {
					if (!found)
						failed[op] = T
					else
						failed = c(plus=F, minus=F)
					break
				}
			}
			if (all(failed))
				break
			op = setdiff(names(failed), op)
		}
	} # end big loop	
	return(path)		
}	
	
	

		
	