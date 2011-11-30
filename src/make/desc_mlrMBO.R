writeDesc <- function(desc.path, rev.nr) {
  sink(file=desc.path)
  cat(
    "Package: mlrMBO\n",
    "Type: Package\n",
    "Title: mlr: Modelbased Optimization.\n",
    paste("Version: 0.1.", rev.nr, "\n", sep=""),
    "Date: 2009-01-01\n",
    "Author: Bernd Bischl\n",
    "Maintainer: Bernd Bischl <bernd_bischl@gmx.net>\n",
    "Description: no\n",
    "License: GPL (>= 2)\n",
    "LazyLoad: yes\n",
    "Depends: R (>= 2.11.0), methods, mlr\n",
    "Suggests: lhs, DiceOptim, cmaes\n",
    sep=""
  )
  sink(NULL)
}
