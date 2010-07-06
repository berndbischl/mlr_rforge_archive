#' @include object.r
roxygen()


setClass(
		"data.desc",
		contains = c("object"),
		representation = representation(
				props = "list"
		)
)


setMethod(
  f = "initialize",
  signature = signature("data.desc"),
  def = function(.Object, data, target, excluded) {
	  i = which(colnames(data) %in% c(target, excluded))
	  df2 = data[, -i, drop=F]
	  
	  .Object@props$target = target 
	  .Object@props$obs = nrow(data)
	  .Object@props$is.classification <- is.factor(data[, target])
	  .Object@props$rows.with.missings <- sum(apply(df2, 1, function(x) any(is.na(x))))
	  .Object@props$cols.with.missings <- sum(apply(df2, 2, function(x) any(is.na(x))))
	  .Object@props$numerics <- sum(sapply(df2, is.numeric))
      .Object@props$integers <- sum(sapply(df2, is.integer))
      .Object@props$factors <- sum(sapply(df2, is.factor))
      .Object@props$characters <- sum(sapply(df2, is.character))
	  if(.Object@props$is.classification)	
	  	.Object@props$classes = levels(data[, target])
	  return(.Object)
  }
)


#' @rdname data.desc-class
setMethod(
		f = "[",
		signature = signature("data.desc"),
		def = function(x,i,j,...,drop) {
			if (i == "size") {
				return(x["obs"])
			}
			if (i == "class.levels") {
				return(x["classes"])
			}
			if (i == "class.nr") {
				return(length(x["class.levels"]))
			}
			if (i == "is.binary") {
				return(x["class.nr"] == 2)
			}
			if (i == "has.missing") {
				return(x["rows.with.missings"] > 0)
			}
			return(x@props[[i]])
			#callNextMethod()
		}
)





#' @rdname to.string
setMethod(
		f = "to.string",
		signature = signature("data.desc"),
		def = function(x) {
			return(
					paste( 
							"Features Nums:", x["numerics"], " Ints:", x["integers"], " Factors:", x["factors"], " Chars:", x["characters"], "\n",
							"Observations: ", x["obs"] , "\n",
							"Missings: ", x["has.missing"], "\n", 
							ifelse(x["has.missing"], paste("in", x["rows.with.missings"], "observations and", x["cols.with.missings"], "features\n"), ""), 
							sep=""
					)
			)
		}
)



