
prepare.df = function(df, class.col, ints.as.nums = TRUE, ints.as.factors=FALSE, chars.as.factors = TRUE)   {
	
	#convert class to factor
	df[, class.col] = as.factor(df[, class.col])

	for (i in 1:ncol(df)) {
		v <- df[,i]
		# convert integers
		if (is.integer(v)) {
			if (ints.as.nums)
				v <- as.numeric(v)
			else if (ints.as.factors)
				v <- as.factor(v)
		}
		# convert integers
		if (is.character(v)) {
			if (chars.as.factors)
				v <- as.factor(v)
		}		
		df[,i] <- v
	}

	return(df)    
}
    