check.task <- function(data, target) {
	cns = colnames(data)
	msg = ""
	
	if (!(target %in% cns)) {
		msg <- paste("Column names of data.frame don't contain target var: ", target)
	}
	
	forbidden = c("[", "]", "(", ")")
	i = sapply(forbidden, function(x) length(grep(x, cns, fixed=T)) > 0)
	if (any(i))
		msg <- paste("Column names should not contain: ", paste(forbidden, collapse=" "))
	
	return(msg)
}
