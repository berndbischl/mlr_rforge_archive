# todo: we could pass costs with extra loss function?
# todo: for ctree_control we should load party as well. pack / packs in learner?

#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
		"classif.blackboost", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.blackboost"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)
			
			x = callNextMethod(.Object, pack=c("mboost", "party"), desc=desc)
			par.descs = list(
					new("par.desc.disc", par.name="family", default="Binomial", vals=list(AdaExp=AdaExp(), Binomial=Binomial())),
					new("par.desc.double", par.name="mstop", default=100L, lower=1L),
					new("par.desc.double", par.name="nu", default=0.1, lower=0, upper=1),
					new("par.desc.disc", par.name="teststat", default="quad", vals=c("quad", "max")),
					new("par.desc.disc", par.name="testtype", default="Bonferroni", vals=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
					new("par.desc.double", par.name="mincriterion", default=0.95, lower=0, upper=1),
					new("par.desc.double", par.name="minsplit", default=20L, lower=1L),
					new("par.desc.double", par.name="minbucket", default=7L, lower=1L),
					new("par.desc.log", par.name="stump", default=FALSE),
					new("par.desc.double", par.name="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
					new("par.desc.double", par.name="maxsurrogate", default=0L, lower=0L),
					new("par.desc.double", par.name="mtry", default=0L, lower=0L),
					new("par.desc.log", par.name="savesplitstats", default=TRUE),
					new("par.desc.double", par.name="maxdepth", default=0L, lower=0L)
			)
			# we have to load the package first for Binomial()
			x@par.descs = par.descs
			set.hyper.pars(x, family="Binomial")
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.blackboost", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {		
			xs = args.to.control(boost_control, c("mstop", "nu", "risk"), list(...))
			ys = args.to.control(ctree_control, c("teststat", "testtype", "mincriterion", "maxdepth"), xs$args)
			f = .task["formula"]
			args = c(list(f, data=.task["data"][.subset, .vars], weights=.weights, control=xs$control, tree_control=ys$control), ys$args)
			do.call(blackboost, args)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.blackboost", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			type = ifelse(.type=="response", "class", "response")
			p = predict(.model["learner.model"], newdata=.newdata, type=type, ...)
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) <- .model["class.levels"]
				y[,1] <- p
				y[,2] <- 1-p
				return(y)
			} else {
				return(p)
			}
		}
)	





