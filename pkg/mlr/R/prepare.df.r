
prep.data = function(data, target.col, data.desc, 
		ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE)   {
	
	if (identical(ints.as.nums, TRUE) && identical(ints.as.factors, TRUE)) {
		stop("ints.as.nums and ints.as.factors both TRUE!")
	}
	
	if ( (is.numeric(ints.as.nums)    || is.integer(ints.as.nums)) &&
		 (is.numeric(ints.as.factors) || is.integer(ints.as.factors)) &&
		 length(intersect(ints.as.nums, ints.as.factors)>0) ) {
	 	
	 	i <- intersect(ints.as.nums, ints.as.factors)
	 	stop(paste("ints.as.nums and ints.as.factors contain the same indices: ", i))
	}
	
	# convert ints to numerics
	if (data.desc@integers > 0) {
		if (ints.as.nums) {
		# only warn when no specific cols where given
		if (is.logical(ints.as.nums) || length(ints.as.nums) == 1)			
			warning("Data set contains int. variables. Converting them to numerics!")
		data[, ints.as.nums] <- as.data.frame(
				lapply(data[, ints.as.nums], function (x) if(is.integer(x)) as.numeric(x) else x),
				stringsAsFactors=FALSE)
		}
	}

	# convert ints to factors
	if (data.desc@integers > 0) {
		if (ints.as.factors) {
			# only warn when no specific cols where given
			if (is.logical(ints.as.factors) || length(ints.as.factors) == 1)			
				warning("Data set contains int. variables. Converting them to factors!")
			data[, ints.as.factors] <- as.data.frame(
					lapply(data[, ints.as.factors], function (x) if(is.integer(x)) as.factor(x) else x),
					stringsAsFactors=FALSE)
		}
	}

	return(data)    
}

prep.classif.data <- function(data, target.col, data.desc, 
		ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE, 
		drop.class.levels=TRUE)   {
	
	
	if (missing(data.desc))
		data.desc <- make.data.desc(data=data, target.col=target.col)
	if(missing(target.col)) 
		target.col <- data.desc@target.col	
	
	#convert target to factor
	if (!is.factor(data[, target.col])) {
		if(is.integer(data[, target.col]) || is.character(data[, target.col])) {
			warning("Converting target col. to factor.")
			data[, target.col] = as.factor(data[, target.col])
		} else {
			stop("Unsuitable target col. for classification data!")				
		}
	}	
	
	# drop unused class levels
	if (drop.class.levels) {
		before.drop <- levels(data[,target.col])
		data[, target.col] <- (data[,target.col])[, drop=TRUE]
		after.drop <- levels(data[,target.col])
		data.desc@class.nr <- length(after.drop) 
				if(!identical(before.drop, after.drop)) {
			warning(paste("Empty levels were dropped from class col.:", 
							setdiff(before.drop, after.drop)))
		}	
	}
	prep.data(data=data, target.col=target.col, data.desc=data.desc, 
			ints.as.nums, ints.as.factors, chars.as.factors)
}
	
