

setClass(
		"opt.control",
		contains = c("object"),
		representation = representation(
				method = "character",
				minimize = "logical",
				tune.threshold= "logical", 
				thresholds = "numeric"
		)
)

