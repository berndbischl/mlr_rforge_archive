
varsel.hybrid = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	
	path = list()
	all.vars = task["input.names"]
	data = task["data"][,all.vars]
	cors = abs(cor(data, data))
	diag(cors) = NA
	m = length(all.vars) 
	flip.rate = control$epsilon
	p01 = control$delta
	# look back for adaption of flip.rate (step.size)
	win.size = 10
	
	start = all.vars[as.logical(rbinom(m, 1, 0.5))]
	#cat("start:", start, "\n")
	state = eval.state(learner, task, resampling, measures, aggr, vars=start, "start")
	path = add.path(path, state, T)		
	#print(get.perf(state))
	
	# big loop for mut + local
	mut.succ = c()
	while (.mlr.vareval < control$maxit) {
		
		# mutate til successful
		vs.bin = all.vars %in% state$vars
		#print(vs.bin)
		while (.mlr.vareval < control$maxit) {
			# we look back win.size mutations and maybe adapt the step.size
			if (length(mut.succ) > win.size) {
				mean.succ = mean(mut.succ[(length(mut.succ)-win.size+1):length(mut.succ)])
				if (mean.succ > 0.3)
					flip.rate = flip.rate/1.2
				if (mean.succ < 0.1)
					flip.rate = flip.rate*1.2
			}
			if (flip.rate > m/2)
				flip.rate = m/2
			probs = abs(as.numeric(vs.bin) - p01) * flip.rate / m
			mut = as.logical(rbinom(m, 1, prob=probs))
			#cat("mut:", mut, "\n")
			# xor
			new.bin = (vs.bin != mut)
			new.vars = all.vars[new.bin]
			#print(new.bin)
			#cat("new.vars:", new.vars, "\n")
			new.state = eval.state(learner, task, resampling, measures, aggr, vars=new.vars, "mutate")
			#print(get.perf(new.state))
			cc = compare.diff(state, new.state, control, measures, aggr, threshold=control$gamma)	&& 
					(length(new.state$vars) > 0)
			path = add.path(path, new.state, cc)
			mut.succ = c(mut.succ, as.numeric(cc))
			if (cc) {
				#print("accept")
				state=new.state
				break
			}
		}
		
		
		
		op = sample(c("plus", "minus"), 1)
		failed = c(plus=F, minus=F)
		while (.mlr.vareval < control$maxit) {
			# try minus or plus repeatedly
			found = F
			while (.mlr.vareval < control$maxit) {
				#print(op)
				if (op == "minus") {
					cors2 = cors[state$vars, state$vars, drop=F]
					meancor = rowMeans(cors2, na.rm=T)
					#cat("mc:", meancor, "\n")
					v = names(which.max(meancor))
					new.vars = setdiff(state$vars, v)
				} else {
					not.used = setdiff(all.vars, state$vars)
					cors2 = cors[not.used, state$vars, drop=F]
					meancor = rowMeans(cors2)
					#cat("mc:", meancor, "\n")
					v = names(which.min(meancor))
					new.vars = c(state$vars, v)
				}
				#cat("newvars:", new.vars, "\n")
				new.state = eval.state(learner, task, resampling, measures, aggr, vars=new.vars, op)
				#print(get.perf(new.state))
				thresh = ifelse(op=="plus", control$alpha, control$beta)
				cc = compare.diff(state, new.state, control, measures, aggr, thresh)
				path = add.path(path, new.state, cc)							
				if (cc) {
					#print("accept")
					state=new.state
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
	list(opt=make.path.el(state), path = path) 
}	
	
	

		
	