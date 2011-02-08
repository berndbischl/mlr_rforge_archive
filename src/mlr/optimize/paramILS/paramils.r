##
## paramils.R - Parameter ILS implementation
##
## Author: Olaf Mersmann <olafm@statistik.tu-dortmund.de>
##
## Based on:
##
##   Frank Hutter, Holger Hoos, and Thomas Stützle.
##   Automatic Algorithm Configuration based on Local Search,
##   In AAAI-07.

require("digest")

printf <- function(x, ...)
  message(sprintf(x, ...))

paramils_fun <- function(fn) {
  cache <<- new.env(TRUE, emptyenv())

  neval <- function(par) {
    key <- digest(par)
    length(cache[[key]])
  }

  teval <- function() {
    sum(sapply(cache, length))
  }
  
  Fn <- function(par, n) {
    key <- digest(par)
    y <- cache[[key]]

    if (missing(n))
      n <- length(y) + 1

    while(length(y) < n) 
      y <- c(y, do.call(fn, par))
    cache[[key]] <<- y
    
    mean(y[1:n])
  }

  attr(Fn, "fmap") <- list(neval=neval,
                           teval=teval
                           )
  class(Fn) <- "paramils_fun"
  Fn
}

neval <- function(Fn, par) {
  if (missing(par))
    attr(Fn, "fmap")$teval()
  else
    attr(Fn, "fmap")$neval(par)
}

##' fils_better is an implementation of the focused ILS 'better than'
##' function.
##'
##' @param Fn a \code{paramils_fun}.
##' @param t1 Parameter settings of challenger
##' @param t2 Parameter settings of incumbant / current best
##' @return \code{TRUE} if \code{t1} is better than \code{t2}, else \code{FALSE}.
##' @export
fils_better <- function(Fn, t1, t2) {
  n <- min(neval(Fn, t1), neval(Fn, t2)) + 1
  y1 <- Fn(t1, n)
  y2 <- Fn(t2, n)

  nmax <- neval(Fn, t2)
  while(neval(Fn, t1) < nmax) {
    if (Fn(t1) < y2) {
      return(FALSE)
    }
  }
  Fn(t1, nmax) < Fn(t2, nmax)
}

##' Perturbe a parameter setting by randomly walking \code{s} steps
##' into its neighbourhood.
##'
##' \code{s} gives the maximal hamming distance between \code{par} and
##' the return value.
##' 
##' @param par Parameter settings
##' @param pars Parameter space description
##' @param s Degree of perturbation
##' @return A new parameter setting.
##' @export
perturbe_parameter <- function(par, pars, s=5) {
  if (s > 1)
    Recall(random_neighbour(par, pars), pars, s-1)
  else
    random_neighbour(par, pars)
}

local_search <- function(Fn,  par, pars) {
  neighbours <- sample(neighbourhood(par, pars))
  for (newpar in neighbours) {
    if (fils_better(Fn, newpar, par))
        return(Recall(Fn, newpar, pars))
  }
  return(par)
}

coalesce <- function(...) {
  l <- list(...)
  isnull <- sapply(l, is.null)
  l[[which.min(isnull)]]
}

parameter_ils_control <- function(maxeval=NULL,
                                  p_restart=NULL,
                                  n_perturb=NULL,
                                  ...,
                                  control=list(),
                                  default=list(maxeval=1000, p_restart=0.1, n_perturb=10)) {
  control$maxeval <- coalesce(maxeval, control[["maxeval"]], default[["maxeval"]])
  control$p_restart <- coalesce(p_restart, control[["p_restart"]], default[["p_restart"]])
  control$n_perturb <- coalesce(n_perturb, control[["n_perturb"]], default[["n_perturb"]])
  control
}

parameter_ils <- function(par, fn, neighbours, control=list()) {
  control <- parameter_ils_control(control=control)
  Fn <- paramils_fun(fn)
  done <- FALSE
  incumbant <- par
  curpar <- incumbant
  while (!done) {
    curpar <- perturbe_parameter(curpar, neighbours, s=control$n_perturb)
    curpar <- local_search(Fn, curpar, pars)
    if (fils_better(Fn, curpar, incumbant))
      incumbant <- curpar
    if (runif(1) < control$p_restart)
      curpar <- random_parameter(pars)
    
    if (neval(Fn) > control$maxeval) {
      done <- TRUE
    }
  }
  list(par=incumbant,
       value=Fn(incumbant))
}
