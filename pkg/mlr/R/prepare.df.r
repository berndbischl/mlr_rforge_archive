
prep.data = function(data, target, data.desc, 
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

prep.classif.data <- function(data, target, data.desc, 
		ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE, 
		drop.class.levels=TRUE)   {

	if (missing(data.desc))
		data.desc <- make.data.desc(data, target)
	
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
		data.desc@class.nr <- length(after.drop) 
				if(!identical(before.drop, after.drop)) {
			warning(paste("Empty levels were dropped from class col.:", 
							setdiff(before.drop, after.drop)))
		}	
	}
	prep.data(data=data, target=target, data.desc=data.desc, 
			ints.as.nums, ints.as.factors, chars.as.factors)
}
	
prep.regr.data <- function(data, target, data.desc, 
		ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE)   {
	
	if (missing(data.desc))
		data.desc <- make.data.desc(data, target)

	prep.data(data=data, target=target, data.desc=data.desc, 
			ints.as.nums, ints.as.factors, chars.as.factors)
	
}


