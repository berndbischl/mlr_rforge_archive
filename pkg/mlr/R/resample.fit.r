#' @include task.learn.r
#' @include resample.instance.r
#' @include resample.result.r
roxygen()



setGeneric(
		name = "resample.fit",
		def = function(learn.task, resample.instance, parset, models, type) {
			if (missing(type))
				type <- "class"
			if (missing(parset))
				parset <- list()
			if (missing(models))
				models <- FALSE
			standardGeneric("resample.fit")
		}
)

#' @export

setMethod(
		f = "resample.fit",
		signature = c(learn.task="learn.task", resample.instance="resample.instance", parset="list", models="logical", type="character"),
		def = function(learn.task, resample.instance, parset, models, type) {
			df <- learn.task@data
			n <- nrow(df)  
			ps <- list()
			ms <- list()
			iters <- resample.instance["iters"]
			
			wrapper <- function(i) {
				resample.fit.iter(learn.task, resample.instance, parset, type, i, return.model=models)
			}
			
			if (.parallel.setup$mode == "snowfall" && .parallel.setup$level == "resample") {
				sfExport("parset")
				if (!is.null(parent.frame()$caller) && !parent.frame()$caller == "tune") {
					sfExport("learn.task")
					sfExport("resample.instance")
				}
				rs <- sfClusterApplyLB(1:iters, wrapper)
			} else {
				rs <- lapply(1:iters, wrapper)
			}
			ps = lapply(rs, function(x) x$pred)
			if (models)			
				ms = lapply(rs, function(x) x$model)
			else
				ms = list()
			return(new("resample.result", ri.name=resample.instance["name"], ri.class=class(resample.instance), preds=ps, models=ms))
		}
)



