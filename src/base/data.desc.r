#' @include object.r
roxygen()


setClass(
		"data.desc",
		contains = c("object"),
		representation = representation(
				target = "character",
				obs = "integer",
				rows.with.missings = "integer",
				cols.with.missings = "integer",
				numerics = "integer",
				integers = "integer",
				factors = "integer",
				characters = "integer",
				is.classification = "logical",	 
				classes = "character"
		)
)


setMethod(
  f = "initialize",
  signature = signature("data.desc"),
  def = function(.Object, data, target, excluded) {
	  i = which(colnames(data) %in% c(target, excluded))
	  df2 = data[, -i]
	  
	  .Object@target = target 
	  .Object@obs = nrow(data)
	  .Object@is.classification <- is.factor(data[, target])
	  .Object@rows.with.missings <- sum(apply(df2, 1, function(x) any(is.na(x))))
	  .Object@cols.with.missings <- sum(apply(df2, 2, function(x) any(is.na(x))))
	  .Object@numerics <- sum(sapply(df2, is.numeric))
      .Object@integers <- sum(sapply(df2, is.integer))
      .Object@factors <- sum(sapply(df2, is.factor))
      .Object@characters <- sum(sapply(df2, is.character))
	  if(.Object@is.classification)	
	  	.Object@classes = levels(data[, target])
	  return(.Object)
  }
)



setMethod(
		f = "[",
		signature = signature("data.desc"),
		def = function(x,i,j,...,drop) {
			
			if (i == "size") {
				return(x@obs)
			}
			if (i == "class.levels") {
				return(x@classes)
			}
			if (i == "class.nr") {
				return(length(x["class.levels"]))
			}
			if (i == "is.binary") {
				return(x["class.nr"] == 2)
			}
			if (i == "has.missing") {
				return(x@rows.with.missings > 0)
			}
			callNextMethod()
		}
)




#' Conversion to string.

setMethod(
		f = "to.string",
		signature = signature("data.desc"),
		def = function(x) {
			return(
					paste( 
							"Features Nums:", x@numerics, " Ints:", x@integers, " Factors:", x@factors, " Chars:", x@characters, "\n",
							"Observations: ", x@obs , "\n",
							"Missings: ", x["has.missing"], "\n", 
							ifelse(x["has.missing"], paste("in", x@rows.with.missings, "observations and", x@cols.with.missings, "features\n"), ""), 
							sep=""
					)
			)
		}
)



setMethod(
  f = "print",
  signature = signature("data.desc"),
  def = function(x, ...) {
    cat(to.string(x))
  }
)


setMethod(
  f = "show",
  signature = signature("data.desc"),
  def = function(object) {
    cat(to.string(object))
  }
)
