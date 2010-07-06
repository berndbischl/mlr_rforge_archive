c.factor = function(...) {
	args <- list(...)
	for (i in seq(along=args)) if (!is.factor(args[[i]])) args[[i]] = 
					as.factor(args[[i]])
	# The first must be factor otherwise we wouldn't be inside c.factor, its checked anyway in the line above.
	newlevels = sort(unique(unlist(lapply(args,levels))))
	ans = unlist(lapply(args, function(x) {
						m = match(levels(x), newlevels)
						m[as.integer(x)]
					}))
	levels(ans) = newlevels
	class(ans) = "factor"
	return(ans)
}


# do lapply recursively on deep lists
rec.lapply = function(xs, fun, depth=Inf) {
	if (!is.list(xs) || is.data.frame(xs) || depth==0) {
		return(fun(xs))
	}
	lapply(xs, function(x) rec.lapply(x, fun, depth-1))
}


# inserts elements from x2 into x1, overwriting elements of equal names
# if el.names contains names which are nor present in x2, they are disregarded
insert = function(xs1, xs2, el.names=names(xs2)) {
	el.names = intersect(el.names, names(xs2))
	xs1[el.names] = xs2[el.names]
	return(xs1)
}

# inserts elements from x2 into x1, only if names in x2 are already present in x1 
insert.matching = function(xs1, xs2) {
	ns = intersect(names(xs1), names(xs2))
	xs1[ns] = xs2[ns]
	return(xs1)
}


# return a list of (...) minus all stuff in arg.names, on which control func. ctrl is called
args.to.control = function(control, arg.names, args) {
	# put stuff into special list and remove it from args
	ctrl.args = insert(list(), args, arg.names)
	ctrl = do.call(control, ctrl.args)
	args[arg.names] = NULL
	return(list(control=ctrl, args=args))
}


check.list.type = function(xs, type, name) {
	if (missing(name))
		name = deparse(substitute(xs))
	fs = lapply(type, function(tt) switch(tt,
			character=is.character,                          
			numeric=is.numeric,
			logical=is.logical,
			integer=is.integer,
			list=is.list,
			data.frame=is.data.frame,
			function(x) is(x, tt)
	))
	types = paste(type, collapse=", ")	
	all(sapply(seq(length=length(xs)), function(i) {
				x = xs[[i]]
				ys = sapply(fs, function(f) f(x))
				if(!any(ys))
					stop("List ", name, " has element of wrong type ", class(x), " at position ", i, ". Should be: ", types)
				any(ys)
	}))
}

#check.list.types = function(name, xs, types) {
#	sapply(types, function(tt) check.list.type(name, xs, tt))
#} 


vote.majority = function(x) {
	tt = table(x)
	y = seq_along(tt)[tt == max(tt)]
	if (length(y) > 1L) 
		y = sample(y, 1L)
	names(tt)[y]
}

# selects the maximal name of the maximal element of a numerical vector - breaking ties at random
vote.max.val = function(x, names=names(x)) {
	y = seq_along(x)[x == max(x)]
	if (length(y) > 1L) 
		y = sample(y, 1L)
	return(names[y])
}


# returns first non null el. 
coalesce = function (...) {
	l <- list(...)
	isnull <- sapply(l, is.null)
	l[[which.min(isnull)]]
}


list2dataframe = function(xs, rownames=NULL) {
	ys = as.data.frame(Reduce(rbind, xs))
	rownames(ys) = rownames
	return(ys)
}


path2dataframe = function(path) {
	p = path[[1]]
	cns = c(names(p$par), "threshold", names(p$perf), "evals", "event", "accept")
	df = matrix(0, length(path), length(cns))
	colnames(df) = cns
	n = length(p$par)
	m = length(p$perf)
	k = ncol(df)
	df = as.data.frame(df)
	df$event = as.character(df$event)
	for (i in 1:length(path)) {
		p = path[[i]]
		df[i, 1:n] = unlist(p$par)  
		df[i, (n+1):(k-2)] = c(p$threshold, unlist(p$perf), p$evals)  
		df[i, k-1] = p$event  
		df[i, k] = p$accept  
	}
	return(df)
}

