
#' Since not all classifiers can deal with all kind of data, the properties of the data are compared 
#' with the supported features of the learner when a \code{\linkS4class{learn.task}} is generated.
#' A \code{data.desc} object contains a description of these data properties.
#' 
#' @slot target.col Column index of the response
#' @slot is.classification Is the target variable categorical?
#' @slot class.nr Does the dataset have missing values?
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
		representation = representation(
				target.col = "integer",
				is.classification = "logical",	 
				class.nr = "integer",
				has.missing = "logical",
				numerics = "integer",
				integers = "integer",
				factors = "integer",
				characters = "integer",
				obs = "integer"
		)
)

#' Constructor.
#' @title data.desc constructor
#' @rdname data.desc-class

setMethod(
  f = "initialize",
  signature = signature("data.desc"),
  def = function(.Object, data, target.col) {
      col <- target.col
      df2 <- data[,-col]
	  .Object@target.col <- col 
	  .Object@is.classification <- is.factor(data[, col]) 
	  .Object@class.nr <- length(levels(data[, col]))
      .Object@has.missing <- any(sapply(data, is.na))
      .Object@numerics <- sum(sapply(df2, is.numeric))
      .Object@integers <- sum(sapply(df2, is.integer))
      .Object@factors <- sum(sapply(df2, is.factor))
      .Object@characters <- sum(sapply(df2, is.character))
	  .Object@obs <- nrow(data)
	  return(.Object)
  }
)


setGeneric(
		name = "make.data.desc",
		def = function(data, target.col) {
			standardGeneric("make.data.desc")
		}
)

setMethod(
		f = "make.data.desc",
		signature = signature(data="data.frame", target.col="integer"),
		def = function(data, target.col) {
			new("data.desc", data=data, target.col=target.col)
		}
)

setMethod(
		f = "make.data.desc",
		signature = signature(data="data.frame", target.col="character"),
		def = function(data, target.col) {
			i = which(colnames(data) == target.col)
			make.data.desc(data=data, target.col=i)
		}
)


#' Conversion to string.
#' @rdname data.desc-class

setMethod(
		f = "as.character",
		signature = signature("data.desc"),
		def = function(x) {
			return(
					paste("Dataset: ", 
							ifelse(x@is.classification, "Classification", "Regression"), " problem\n",
							ifelse(x@is.classification, paste("Classes:", x@class.nr, "\n"), ""),
							"Features Nums:", x@numerics, " Ints:", x@integers, " Factors:", x@factors, " Chars:", x@characters, "\n",
							"Observations: ", x@obs , "\n",
							"Missings: ", x@has.missing, "\n", sep=""
					)
			)
		}
)


#' Prints the object by calling as.character.
#' @rdname data.desc-class

setMethod(
  f = "print",
  signature = signature("data.desc"),
  def = function(x, ...) {
    cat(as.character(x))
  }
)

#' Shows the object by calling as.character.
#' @rdname data.desc-class

setMethod(
  f = "show",
  signature = signature("data.desc"),
  def = function(object) {
    cat(as.character(object))
  }
)
