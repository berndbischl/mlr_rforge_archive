check.task <- function(data, target) {
	cns = colnames(data)

	if (!(target %in% cns)) {
		stop(paste("Column names of data.frame don't contain target var: ", target))
	}
	
	# todo: rpart does not like (), bug there?
	#forbidden = c("[", "]", "(", ")")
	forbidden = c("[", "]")
	i = sapply(forbidden, function(x) length(grep(x, cns, fixed=T)) > 0)
	if (any(i))
		stop(paste("Column names should not contain: ", paste(forbidden, collapse=" ")))
}
