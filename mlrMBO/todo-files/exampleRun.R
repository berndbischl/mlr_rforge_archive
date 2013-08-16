library(denstrip)
library(soobench)
library(testthat)
library(devtools)
library(DAAG)
library(mlr)

load_all("skel", reset=TRUE)
configureMlr(show.learner.output=FALSE, on.learner.error="stop")

#' Perform a model based optimization on a 1D function and and visualize what happens.
#'
#' Run \code{plot} on the resulting object.
#' Useful for figuring out how stuff works and for teaching purposes.
#'
#' The plot will show the following elements per iteration:
#' (a) Above plot
#' - The true objective function (solid line).
#' - The surrogate approximation, represented by its mean response (dotted line).
#' - Surrogate mean +- 1 standard deviation, from local uncertainty (dotted line).
#' (b) Lower plot
#' - Infill criterion.
#' 
#' In both plots the following elements are present
#' - Initial design points (black)
#' - Points from previous sequentail iteraions (green)
#' - Proposed point in current iteration. 
#'
#' @param fun [\code{function}]\cr
#'   Objective function. 
#'   First argument must be a numeric decision variable.
#'   The function has to return a single numerical value.
#' @param surrogate [\code{\link[mlr]{Learner}}]\cr
#'   Surrogate model used for the optimization of \code{fun}.
#FIXME regr.km might be renamed
#'   Default is mlr learner \dQuote{regr.km}, which is kriging from package
#'   DiceKriging. \code{nugget.estim} is set to \code{TRUE} depending on whether we have 
#'   noisy observations or not.
#' @param control [\code{\link{MBOControl}}]\cr
#'   MBO control object.
#' @param evals [\code{integer(1)}]\cr
#'   Number of evaluations if \code{fun} is noisy.
#'   Default is 10 for noisy functions
#' @param n [\code{integer(1)}]\nr
#'   Number of locations at which to sample the \code{fun} function.
#' @return [\code{list}]:
#'   \item{xseq [\code{numeric}]}{Sequence of x values from the domain of \code{fun}.}
#'   \item{yseq [\code{numeric}]}{Sequence of evaluated points.}
#'   \item{ymat [\code{numeric}]}{Sequence of evaluated points.}
#'   \item{mbo.res [\code{\link{MBOResult}}]}{MBO result object.}
#'   \item{par.set [\code{\link[ParamHelpers]{ParamSet}}]}{See argument.}
#'   \item{learner [\code{\link[mlr]{Learner}}]}{See argument.}
#'   \item{control [\code{\link{MBOControl}}]}{See argument.}
exampleRun = function(fun, par.set, learner, control, noisy.evals = 5, n = 50) {
  checkArg(fun, "function")
  if (missing(learner)) {
    learner = makeLearner("regr.km", predict.type = "se", nugget.estim = control$noisy)
  } else {
    checkArg(learner, "Learner")
  }
  noisy.evals = convertInteger(noisy.evals)
  n.iters = control$n.iters
  checkArg(noisy.evals, "integer", len = 1L, na.ok = FALSE)
  n = convertInteger(n)
  checkArg(n, "integer", len = 1L, na.ok = FALSE)

  control$save.model.at=0:n.iters
  name.x = getParamIds(par.set)
  name.y = control$y.name
  
  #show some info on console
  messagef("Peforming MBO on function.")
  messagef("Initial design: %i. Sequential iterations: %i.", control$n.init.design.points, control$n.iters)
  messagef("Learner: %s. Settings:\n%s", learner$id, mlr:::getHyperParsString(learner))
  
  # extract bounds for fun
  lower = getLower(par.set)
  upper = getUpper(par.set)
  
  #FIXME maybe allow 1 discrete or int param as well!
  par.set = makeParamSet(makeNumericParam(name.x, lower = lower, upper = upper))
  
  res = mbo(fun, par.set, learner = learner, control = control)
  
  xseq = seq(lower, upper, length.out = n)

  # check if noisy
  yseq = sapply(xseq, function(x) {
    if(control$noisy) {
      mean(replicate(noisy.evals, fun(list(x=x))))
    } else {
      fun(list(x=x))
    }
  })

  structure(list(xseq = xseq, yseq=yseq, name.x=name.x, name.y=name.y, 
    learner=learner, control=control, mbo.res=res), class="MBOExampleRun")
}

#FIXME x and y labels are not shown
plot.MBOExampleRun = function(obj, ...) {
  par(mfrow=c(2, 1))

  # extract information from example run object
  name.x = obj$name.x
  name.y = obj$name.y
  xseq = obj$xseq
  yseq = obj$yseq
  iters = obj$control$n.iters
  name.crit = obj$control$infill.crit
  
  finegrid = data.frame(x=xseq, y=yseq)
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }
  
  op = as.data.frame(obj$mbo.res$opt.path)
  # ind.* are index sets into the opt.path (initial design and so on)
  ind.inides = which(op$dob == 0)
  
  plotDesignPoints = function(op, ind.inides, ind.seqdes, ind.prodes, y) {
    points(op[ind.inides, name.x], op[ind.inides, y], pch = 19, col = "black")
    points(op[ind.seqdes, name.x], op[ind.seqdes, y], pch = 19, col = "darkseagreen")
    points(op[ind.prodes, name.x], op[ind.prodes, y], pch = 19, col = "tomato")
  }
  
  for (i in seq_len(iters)) {
    mod = obj$mbo.res$models[[i]]
    
    ind.seqdes = which(op$dob > 0 & op$dob < i)
    ind.prodes = which(op$dob == i)
    ind.pasdes = c(ind.inides, ind.seqdes)
    
    # compute model prediction for current iter
    finegrid$yhat = mlrMBO:::infillCritMeanResponse(finegrid[, "x", drop =FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    finegrid$se = -mlrMBO:::infillCritStandardError(finegrid[, "x", drop =FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    finegrid$crit = opt.direction * critfun(finegrid[, "x", drop =FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    finegrid$yhat.low = finegrid$yhat - 1 * finegrid$se 
    finegrid$yhat.upp = finegrid$yhat + 1 * finegrid$se

    # ggplot playground 
    # library(ggplot2)
    # library(gridExtra)
    # finegrid2 = data.frame(x=rep(finegrid$x, 2))
    # finegrid2$y = c(finegrid$y, finegrid$yhat)
    # finegrid2$f = rep(c("y", "yhat"), each=nrow(finegrid))
    # finegrid2$yhat.low = rep(finegrid$yhat.low, 2)
    # finegrid2$yhat.upp = rep(finegrid$yhat.upp, 2)
    # #print(head(finegrid2))
    # # FIXME: insert infill crit in legend, place legend
    # pl1 = ggplot(data=finegrid2, aes(x=x, y=y, linetype=f)) + geom_line()
    # pl1 = pl1 + ggtitle(paste("Iteration", i))
    # pl1 = pl1 + theme(legend.position="none")
    # pl1 = pl1 + geom_ribbon(aes(ymin=yhat.low, ymax=yhat.upp), alpha=0.1, colour ="gray", linetype="dashed", fill="red")
    # pl2 = ggplot(data=finegrid, aes(x=x, y=crit)) + geom_line(linetype="dashed")
    # grid.arrange(pl1, pl2, nrow=2)
    # stop()
    
    # infill crit y vals for lower plot
    op[[name.crit]] = opt.direction * critfun(op[, name.x, drop =FALSE], 
      mod, ctrl, par.set, op[ind.pasdes, ])
    
    # define layout, i.e., the space available and the order of the plots 
    layout(matrix(c(1,2,3), ncol=1, byrow=TRUE), heights=c(2.25, 2.25, 0.5))

    par(mai=c(0.15,0.8,0.3,0.2))
    plot(c(), xlim = range(xseq), ylim = range(yseq), 
      xlab = obj$name.x, ylab = name.y, main = sprintf("Iter = %i", i))

    dr1 = seq(length = 200, 
      min(finegrid$yhat - 1.5 * finegrid$se), 
      max(finegrid$yhat + 1.5 * finegrid$se)
    )
    dr2 = matrix(nrow = length(xseq), ncol = length(dr1))
    for(i in seq_along(xseq)) 
      dr2[i,] = dnorm(dr1, finegrid$yhat[i], finegrid$se[i])
    densregion(xseq, dr1, dr2, pointwise = TRUE, colmax = "pink", )
    lines(xseq, finegrid$yhat.low, lty = "dotted", lwd = 1, col = rgb(0, 0, 0, alpha = 0.5))
    lines(xseq, finegrid$yhat.upp, lty = "dotted", lwd = 1, col = rgb(0, 0, 0, alpha = 0.5))
    lines(xseq, yseq, lwd = 1)
    lines(finegrid$x, finegrid$yhat, lty = "dotted", lwd = 1)
    #FIXME what about noise on real evals during mbo? show this how? plot real evals??
    plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.y)
    
    par(mai=c(0.15,0.8,0.15,0.2))
    plot(xseq, finegrid$crit, type = "l", lty = "dashed", 
      xlab = name.x, ylab=name.crit, lwd=1)
    plotDesignPoints(op, ind.inides, ind.seqdes, ind.prodes, name.crit)
    
    #FIXME show design points

    # add legend in seperate layout row
    par(mai=c(0,0,0.2,0))
    plot.new()
    legend(x="center", ncol = 3, border="white", legend=c("y", expression(hat(y)), name.crit), lty=c("solid", "dotted", "dashed"))
    pause()
  }
}

set.seed(1)


# Some 
objfun = function(x) {
  sum(x$x*x$x)
}

objfun2 = function(x) {
  x = x$x
  (6*x - 2)^2 * sin(12 * x - 4)
}

par.set = makeNumericParamSet(id="x", len=1, lower=0, upper=1)

ctrl = makeMBOControl(noisy=FALSE, n.init.design.points=4, n.iters=2, 
   infill.crit="ei", infill.opt="random", random.n.points=1000
)

z = exampleRun(objfun2, par.set, control=ctrl, n = 500)
plot(z)

#braninfun = generate_branin_function()
#z = exampleRun(makeMBOFunction(braninfun), lower=lower_bounds(braninfun), upper=upper_bounds(braninfun), dimension = 2, control = control)
#plot.ExampleRun(z)