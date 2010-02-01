
prep.data = function(data, target, excluded=c(), ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE)   {
	
	if (identical(ints.as.nums, TRUE) && identical(ints.as.factors, TRUE)) {
		stop("ints.as.nums and ints.as.factors both TRUE!")
	}
	
	cns = colnames(data)
	for (i in 1:ncol(data)) {
		cn = cns[i]
		v = data[, i]
		if (!(cn  %in% excluded)) {
			if (ints.as.nums && is.integer(v)) {
				data[,i] = as.numeric(v)
				warning("Converting integer variable to numeric:", cn)
			}
			if (ints.as.factors && is.integer(v)) {
				data[,i] = as.factor(v)
				warning("Converting integer variable to factor:", cn)
			}
			if (chars.as.factors && is.character(v)) {
				data[,i] = as.factor(v)
				warning("Converting char variable to factor:", cn)
			}
		}
	}
	
	return(data)    
}

prep.classif.data <- function(data, target, excluded=c(), 
		ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE, 
		drop.class.levels=TRUE)   {

	targets = data[, target]
	
	#convert target to factor
	if (!is.factor(targets)) {
		if(is.integer(data[, target]) || is.character(targets)) {
			warning("Converting target col. to factor.")
			data[, target] = as.factor(targets)
		} else {
			stop("Unsuitable target col. for classification data!")				
		}
	}	
	
	targets = data[, target]
	
	# drop unused class levels
	if (drop.class.levels) {
		before.drop <- levels(targets)
		data[, target] <- targets[, drop=TRUE]
		after.drop <- levels(data[, target])
		if(!identical(before.drop, after.drop)) {
			warning(paste("Empty levels were dropped from class col.:", 
							setdiff(before.drop, after.drop)))
		}	
	}
	prep.data(data=data, target=target, excluded,
			ints.as.nums, ints.as.factors, chars.as.factors)
}
	
prep.regr.data <- function(data, target, excluded=c(),
		ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE)   {
	
	prep.data(data=data, target=target, excluded,
			ints.as.nums, ints.as.factors, chars.as.factors)
	
}


