check.task <- function(data, target) {
	cns = colnames(data)
	msg = ""
	
	if (!(target %in% cns)) {
		msg <- paste("Column names of data.frame don't contain target var: ", target)
	}
	return(msg)
}
