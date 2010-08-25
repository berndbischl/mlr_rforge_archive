##
## parameter_space.R - Parameter space descriptions
##
## Author: Olaf Mersmann <olafm@statistik.tu-dortmund.de>
##
random_parameter <- function(pspace, ...)
  UseMethod("random_parameter")

random_neighbour <- function(par, pspace, ...)
  UseMethod("random_neighbour", pspace)

random_neighbour.default <- function(par, pspace, ...)
  sample(neighbourhood(par, pspace))[[1]]

neighbourhood <- function(par, pspace, ...)
  UseMethod("neighbourhood", pspace)

################################################################################
discrete_parameter_space <- function(..., list=NULL) {
  pars <- if (missing(list))
    list(...)
  else
    c(list, ...)

  class(pars) <- "discrete_parameter_space"
  pars
}

random_parameter.discrete_parameter_space <- function(pspace, ...)
  lapply(pspace, sample, size=1)

random_neighbour.discrete_parameter_space <- function(par, pspace, ...) {
  repeat {
    d <- sample(length(pspace), 1L)
    if (length(pspace[[d]]) > 1L)
      break
  }
  values <- pars[[d]]
  cpos <- which(values == par[[d]])
  for (dir in sample(c(-1,1))) {
    npos <- cpos + dir
    ## Valid move?
    if (npos >= 1 && npos <= length(values)) {
      newpar <- par
      newpar[[d]] <- values[npos]
      return(newpar)
    }
  }
}

neighbourhood.discrete_parameter_space <- function(par, pspace, ...) {
  D <- 1:length(pspace)
  res <- list()
  for (d in D) {
    values <- pspace[[d]]
    cpos <- which(values == par[[d]])
    for (dir in c(-1,1)) {
      npos <- cpos + dir
      ## Valid move?
      if (npos >= 1 && npos <= length(values)) {
        newpar <- par
        newpar[[d]] <- values[npos]
        res <- append(res, list(newpar))
      }
    }
  }
  res
}  

################################################################################
constrained_parameter_space <- function(pspace, is_valid) {
  structure(list(pspace=pspace,
                 is_valid=is_valid),
            class="constrained_parameter_space")
}

random_parameter.constrained_parameter_space <- function(pspace, ...) {
  repeat {
    par <- random_parameter(pspace$pspace)
    if (pspace$is_valid(par))
      return(par)
  }
}

random_neighbour.constrained_parameter_space <- function(par, pspace, ...) {
  repeat {
    par <- random_neighbour(par, pspace, ...)
    if (pspace$is_valid(par))
      return(par)
  }
}

neighbourhood.constained_parameter_space <- function(par, pspace, ...) {
  nb <- neighbourhood(par, pspace$pspace)
  Filter(pspace$is_valid, nb)
}
