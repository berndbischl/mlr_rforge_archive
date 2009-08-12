
#' Since not all classifiers can deal with all kind of data, the properties of the data are compared 
#' with the supported features of the learner when a \code{\linkS4class{learn.task}} is generated.
#' A \code{data.desc} object contains a description of these data properties.

#' @slot is.classification Is the target variable categorical?
#' @slot class.nr Does the dataset have missing values?
#' @slot has.missing Does the dataset have missing values?
#' @slot numerics Does the dataset have numeric variables?
#' @slot integers Does the dataset have integer variables?
#' @slot factors Does the dataset have factor variables?
#' @slot characters Does the dataset have character variables?
#' @slot obs Number of examples in dataset.

#' @exportClass data.desc
#' @title data.desc


setClass(
		"data.desc",
		representation = representation(
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

setMethod(
  f = "initialize",
  signature = signature("data.desc"),
  def = function(.Object, df, target.name) {
      col <- which(names(df) == target.name)
      df2 <- df[,-col]
	  .Object@is.classification <- is.factor(df[, col]) 
	  .Object@class.nr <- length(levels(df[,target.name]))
      .Object@has.missing <- any(sapply(df, is.na))
      .Object@numerics <- sum(sapply(df2, is.numeric))
      .Object@integers <- sum(sapply(df2, is.integer))
      .Object@factors <- sum(sapply(df2, is.factor))
      .Object@characters <- sum(sapply(df2, is.character))
	  .Object@obs <- nrow(df)
	  return(.Object)
  }
)

setMethod(
  f = "print",
  signature = signature("data.desc"),
  def = function(x, ...) {
  }
)

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


setMethod(
  f = "print",
  signature = signature("data.desc"),
  def = function(x, ...) {
    cat(as.character(x))
  }
)

setMethod(
  f = "show",
  signature = signature("data.desc"),
  def = function(object) {
    cat(as.character(object))
  }
)
