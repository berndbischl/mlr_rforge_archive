library(mlrMBO)
library(denstrip)
library(DAAG)
library(soobench)

#' Performs a model based optimization.
#' Useful for teaching purposes in combination with the plot function.
#'
#' @param fun [\code{function}]\cr
#'   Fitness function to minimize. The first argument has to be a list of values.
#'   The function has to return a single numerical value.
#' @param lower [\code{numeric}]\cr
#'   Lower bound for function values.
#' @param upper [\code{numeric}]\cr
#'   Upper bound for function values.
#' @param name.x [\code{character(1)}]\cr
#'   Identifier of the x-values.
#' @param name.y [\code{character(1)}]\cr
#'   Identifier for function values.
#' @param surrogate [\code{\link[mlr]{Learner}}]\cr
#'   Surrogate model used for the optimization of \code{fun}.
#' @param control [\code{\link{MBOControl}}]\cr
#'   MBO control object.
#' @param noisy.evals [\code{numeric(1)}]\cr
#'   Number of evaluations if \code{fun} is noisy.
#' @param n [\code{integer(1)}]\nr
#'   Number of locations at which to sample the \code{fun} function.
#'
#' @return [\code{list}]:
#'   \item{xseq [\code{numeric}]}{Sequence of x values from the domain of \code{fun}.}
#'   \item{yseq [\code{numeric}]}{Sequence of evaluated points.}
#'   \item{name.x [\code{character}]}{Identifier for domain space.}
#'   \item{name.y [\code{character}]}{Identifier for evaluated points.}
#'   \item{surrogate [\code{\link[mlr]{Learner}}]}{Surrogate model used for optimization of \code{fun}.}
#'   \item{control [\code{\link{MBOControl}}]}{MBO control object.}
#'   \item{mbo.res [\code{\link{MBOResult}}]}{MBO result object.}
exampleRun = function(fun, lower, upper, name.x = "x", name.y = "y",
  surrogate, control, noisy.evals = 5, n = 50) {
  
  checkArg(fun, "function")
  checkArg(name.x, "character", len = 1L, na.ok = FALSE)
  checkArg(name.y, "character", len = 1L, na.ok = FALSE)
  checkArg(lower, "numeric", len = 1L, na.ok = FALSE)
  checkArg(upper, "numeric", len = 1L, na.ok = FALSE)
  checkArg(noisy.evals, "numeric", len = 1L, na.ok = FALSE)
  checkArg(n, "numeric", len = 1L, na.ok = FALSE)
  
  if (missing(surrogate)) {
    surrogate = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)
  } else {
    checkArg(surrogate, "Learner")
  }

  #FIXME maybe allow 1 discrete or int param as well!
  par.set = makeParamSet(makeNumericParam(name.x, lower = lower, upper = upper))
  
  res = mbo(fun, par.set, learner = surrogate, control = control)
  
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
    surrogate=surrogate, control=control, mbo.res=res), class="MBOExampleRun")
}

plot.MBOExampleRun = function(obj, ...) {
  par(mfrow=c(2, 1))
  # extract optimization path
  df = as.data.frame(obj$mbo.res$opt.path)

  # extract information from example run object
  name.x = obj$name.x
  name.y = obj$name.y
  xseq = obj$xseq
  yseq = obj$yseq
  n.iters = obj$control$n.iters

  xpoints = data.frame(x = xseq)

  # determine initial design
  initdes = subset(df, dob == 0)

  for (i in 1:obj$control$n.iters) {
    seqdes = subset(df, dob > 0 & dob < i)
    propdes = subset(df, dob == i)
    mod = obj$mbo.res$models[[i]]

    yhat = mlrMBO:::infillCritMeanResponse(xpoints, mod, ctrl, par.set, rbind(initdes, seqdes))
    se = -mlrMBO:::infillCritStandardError(xpoints, mod, ctrl, par.set, rbind(initdes, seqdes))
    ei = -mlrMBO:::infillCritEI(xpoints, mod, ctrl, par.set, rbind(initdes, seqdes))
    proposed.x = xseq[which.max(ei)]
    yhat.low1 = yhat - 1 * se 
    yhat.upp1 = yhat + 1 * se 
    
    layout(matrix(c(1,2,3), ncol=1, byrow=TRUE), heights=c(2.25, 2.25, 0.5))

    par(mai=c(0.15,0.4,0.3,0.2))
    plot(c(), xlim = range(xseq), ylim = range(yseq), 
         xlab = obj$name.x, ylab = name.y, main = sprintf("Iter = %i", i))
    #grid(lty=3, col="lightgray")

    dr1 = seq(min(yhat - 1.5 * se), max(yhat + 1.5 * se), length = 200)
    dr2 = matrix(nrow = length(xseq), ncol = length(dr1))
    for(i in seq_along(xseq)) dr2[i,] <- dnorm(dr1, yhat[i], se[i])
    densregion(xseq, dr1, dr2, pointwise = TRUE, colmax = "pink", )
    lines(xseq, yhat.low1, lty = "dotted", lwd = 1, col = rgb(0, 0, 0, alpha = 0.5))
    lines(xseq, yhat.upp1, lty = "dotted", lwd = 1, col = rgb(0, 0, 0, alpha = 0.5))
    #col1 = rgb(0.7, 0, 0.3, alpha=0.2)
    #col2 = rgb(0.7, 0, 0.3, alpha=0.1)
    #polygon(c(xseq, rev(xseq)), c(yhat.low1, rev(yhat.upp1)), col = col, border = FALSE)  
    #polygon(c(xseq, rev(xseq)), c(yhat.low2, rev(yhat.upp2)), col = col, border = FALSE)  
    lines(xseq, yseq, lwd = 1)
    abline(v = proposed.x)
    lines(xseq, yhat, lty = "dotted", lwd = 1)
    points(initdes[, name.x], initdes[, name.y], pch = 19, col = "black")
    points(seqdes[, name.x], seqdes[, name.y], pch = 19, col = "darkseagreen")
    points(propdes[, name.x], propdes[, name.y], pch = 19, col = "tomato")
        
    par(mai=c(0.15,0.4,0.15,0.2))
    plot(xseq, ei, type = "l", lty = "dashed", xlab = name.x, ylab = "EI", lwd = 1)
    #FIXME show design points
    abline(v = proposed.x)

    # add legend in seperate layout row
    par(mai=c(0,0,0.2,0))
    plot.new()
    legend(x = "center", ncol = 3, legend = c("y", "y_hat", "EI"), lty = c("solid", "dotted", "dashed"))
    pause()
  }
}

n.iters = 8
ctrl = makeMBOControl(noisy = FALSE, n.init.design.points = 4, n.iters = n.iters, 
   infill.crit = "ei", infill.opt = "random", random.n.points = 1000, 
   save.model.at = 0:n.iters)

# Some 
objfun = function(x) {
  sum(x$x*x$x)
}

objfun2 = function(x) {
  x = x$x
  (6*x - 2)^2 * sin(12 * x - 4)
}

z = exampleRun(objfun2, lower=0, upper=1, control=ctrl)
plot(z)

#braninfun = generate_branin_function()
#z = exampleRun(makeMBOFunction(braninfun), lower=lower_bounds(braninfun), upper=upper_bounds(braninfun), dimension = 2, control = control)
#plot.ExampleRun(z)