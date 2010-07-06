#' @include object.r
roxygen()
#' @include learner.desc.r
roxygen()

#' Wraps an already implemented learning method from R to make it accessible to mlr.
#'  
#' Getter.\cr
#' 
#' @exportClass rlearner
#' @title Base class for inducers. 

setClass(
		"rlearner",
		contains = c("learner"),
		representation = representation(
				id = "character",
				label = "character",
				pack = "character",
				desc = "learner.desc",
				predict.type = "character"
		)
)

#' Getter.
#' @rdname rlearner-class

setMethod(
		f = "[",
		signature = signature("rlearner"),
		def = function(x,i,j,...,drop) {
			if (i == "is.classif") {
				return(is(x, "rlearner.classif"))
			}
			if (i == "is.regr") {
				return(is(x, "rlearner.regr"))
			}
			if (i == "pack") {
				return(x@pack)
			}
			if (i == "probs") {
				return(ifelse(x["is.regr"], F, x@desc["probs"]))
			}
			if (i == "decision") {
				return(ifelse(x["is.regr"], F, x@desc["decision"]))
			}
			if (i == "multiclass") {
				return(ifelse(x["is.regr"], F, x@desc["multiclass"]))
			}
			if (i == "missings") {
				return(x@desc["missings"])
			}
			if (i == "costs") {
				return(ifelse(x["is.regr"], F, x@desc["costs"]))
			}
			if (i == "weights") {
				return(x@desc["weights"])
			}
			if (i == "numerics") {
				return(x@desc["numerics"])
			}
			if (i == "factors") {
				return(x@desc["factors"])
			}
			if (i == "characters") {
				return(x@desc["characters"])
			}
			if (i == "pack") {
				return(x@pack)
			}
			if (i == "predict.threshold") {
				return(x["hyper.pars"]$predict.threshold)
			}
			callNextMethod()
		}
)

#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title rlearner constructor
setMethod(
		f = "initialize",
		signature = signature("rlearner"),
		def = function(.Object, id, label, pack, desc, parset.train=list(), parset.predict=list()) {
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			
			if (missing(desc))
				return(.Object)
			if (missing(id))
				id = as.character(class(.Object))
			if (missing(label))
				label = id
			.Object@id = id
			.Object@label = label
			.Object@pack = pack
			.Object@desc = desc
			.Object@predict.type = "response"
			if(pack != "mlr" && !require(pack, character.only=TRUE)) {
				stop(paste("Learner", id, "could not be constructed! package", pack, "missing!"))
			}
			callNextMethod(.Object)
			.Object = set.hyper.pars(.Object, type="train", parset=parset.train)
			.Object = set.hyper.pars(.Object, type="predict", parset=parset.predict)
			return(.Object)
		}
)

#' Base class for classification learners.
#' @exportClass rlearner.classif
#' @title Base class for classification learners. 
setClass("rlearner.classif", contains = c("rlearner"))


#' Base class for regression learners.
#' @exportClass rlearner.regr
#' @title Base class for regression learners. 
setClass("rlearner.regr", contains = c("rlearner"))

