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

# inserts elements from x2 into x1, overwriting elements of equal names 
insert = function(xs1, xs2) {
	xs1[names(xs2)] = xs2
	return(xs1)
}

# inserts elements from x2 into x1, only if names in x2 are already present in x1 
insert.matching = function(xs1, xs2) {
	ns = intersect(names(x1), names(xs2))
	xs1[ns] = xs2[ns]
	return(xs1)
}



# returns first non null el. 
coalesce = function (...) {
	l <- list(...)
	isnull <- sapply(l, is.null)
	l[[which.min(isnull)]]
}
