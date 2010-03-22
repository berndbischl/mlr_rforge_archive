
#' Since not all classifiers can deal with all kind of data, the properties of the data are compared 
#' with the supported features of the learner when a \code{\linkS4class{learn.task}} is generated.
#' A \code{data.desc} object contains a description of these data properties.
#' 
#' @slot target.col Column index of the response
#' @slot is.classification Is the target variable categorical?
#' @slot class.nr Does the dataset have missing values?
#' @slot has.missing Does the dataset have missing values?
#' @slot rows.with.missings Number of rows with NAs
#' @slot cols.with.missings Number of columns with NAs
#' @slot has.missing Does the dataset have missing values?
#' @slot numerics Does the dataset have numeric variables?
#' @slot integers Does the dataset have integer variables?
#' @slot factors Does the dataset have factor variables?
#' @slot characters Does the dataset have character variables?
#' @slot obs Number of examples in dataset.
#' 
#' @exportClass data.desc
#' @title data.desc


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

#' Constructor.
#' @title data.desc constructor

setMethod(
  f = "initialize",
  signature = signature("data.desc"),
  def = function(.Object, data, target) {
      col <- which(colnames(data) == target)
      df2 <- data[,-col]
	  .Object@target = target 
	  .Object@obs = nrow(data)
	  .Object@is.classification <- is.factor(data[, col])
	  .Object@rows.with.missings <- sum(apply(df2, 1, function(x) any(is.na(x))))
	  .Object@cols.with.missings <- sum(apply(df2, 2, function(x) any(is.na(x))))
	  .Object@numerics <- sum(sapply(df2, is.numeric))
      .Object@integers <- sum(sapply(df2, is.integer))
      .Object@factors <- sum(sapply(df2, is.factor))
      .Object@characters <- sum(sapply(df2, is.character))
	  if(.Object@is.classification)	
	  	.Object@classes = levels(data[, col])
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


#' Prints the object by calling as.character.

setMethod(
  f = "print",
  signature = signature("data.desc"),
  def = function(x, ...) {
    cat(to.string(x))
  }
)

#' Shows the object by calling as.character.

setMethod(
  f = "show",
  signature = signature("data.desc"),
  def = function(object) {
    cat(to.string(object))
  }
)
