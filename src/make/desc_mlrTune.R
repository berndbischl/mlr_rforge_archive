writeDesc <- function(desc.path, rev.nr) {
  sink(file=desc.path)
  cat(
    "Package: mlrTune\n",
    "Type: Package\n",
    "Title: mlr: Optimization and Tuning.\n",
    paste("Version: 0.1.", rev.nr, "\n", sep=""),
    "Date: 2009-01-01\n",
    "Author: Bernd Bischl\n",
    "Maintainer: Bernd Bischl <bernd_bischl@gmx.net>\n",
    "Description: no\n",
    "License: GPL (>= 2)\n",
    "LazyLoad: yes\n",
    "Depends: R (>= 2.12.0), mlr\n",
    "Suggests: multicore, snowfall, Rmpi, lhs, cmaes, SPOT, DiceOptim\n",
    sep=""
  )
  sink(NULL)
}
