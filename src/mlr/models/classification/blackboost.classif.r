# todo: we could pass costs with extra loss function?
# todo: for ctree_control we should load party as well. pack / packs in learner?

#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.blackboost", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.blackboost"),
		def = function(.Object) {
			
			
			.Object = callNextMethod(.Object, pack=c("mboost", "party"))
      
      .Object = setProperties(.Object, 
        twoclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
      
			par.set = makeParameterSet(
					makeDiscreteLearnerParameter(id="family", default="Binomial", vals=list(AdaExp=AdaExp(), Binomial=Binomial())),
          makeIntegerLearnerParameter(id="mstop", default=100L, lower=1L),
					makeNumericLearnerParameter(id="nu", default=0.1, lower=0, upper=1),
					makeDiscreteLearnerParameter(id="teststat", default="quad", vals=c("quad", "max")),
					makeDiscreteLearnerParameter(id="testtype", default="Bonferroni", vals=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
					makeNumericLearnerParameter(id="mincriterion", default=0.95, lower=0, upper=1),
          makeIntegerLearnerParameter(id="minsplit", default=20L, lower=1L),
          makeIntegerLearnerParameter(id="minbucket", default=7L, lower=1L),
					makeLogicalLearnerParameter(id="stump", default=FALSE),
          makeIntegerLearnerParameter(id="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
          makeIntegerLearnerParameter(id="maxsurrogate", default=0L, lower=0L),
          makeIntegerLearnerParameter(id="mtry", default=0L, lower=0L),
					makeLogicalLearnerParameter(id="savesplitstats", default=TRUE),
          makeIntegerLearnerParameter(id="maxdepth", default=0L, lower=0L)
			)
     
			# we have to load the package first for Binomial()
      .Object@par.set = par.set
			setHyperPars(.Object, family="Binomial")
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.blackboost", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {		
			xs = args.to.control(boost_control, c("mstop", "nu", "risk"), list(...))
			ys = args.to.control(ctree_control, c("teststat", "testtype", "mincriterion", "maxdepth"), xs$args)
			f = .task["formula"]
      args = c(list(f, data=get.data(.task, .subset), control=xs$control, tree_control=ys$control), ys$args)
			if (.task["has.weights"])
        args$weights = .task["weights"][.subset] 
			do.call(blackboost, args)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.blackboost", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			type = ifelse(.type=="response", "class", "response")
			p = predict(.model@learner.model, newdata=.newdata, type=type, ...)
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) <- getClassLevels(.model)
				y[,1] <- p
				y[,2] <- 1-p
				return(y)
			} else {
				return(p)
			}
		}
)	





