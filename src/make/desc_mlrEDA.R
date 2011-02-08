#todo: what if rev numbers change only because of update of other package?
writeDesc <- function(desc.path, rev.nr) {
  sink(file=desc.path)
  cat(
    "Package: mlrEDA\n",
    "Type: Package\n",
    "Title: mlr: Exploratory Data Analysis\n",
    paste("Version: 0.1.", rev.nr, "\n", sep=""),
    "Date: 2009-01-01\n",
    "Author: Bernd Bischl\n",
    "Maintainer: Bernd Bischl <bernd_bischl@gmx.net>\n",
    "Description: no\n",
    "License: GPL (>= 2)\n",
    "LazyLoad: yes\n",
    "Depends: R (>= 2.8.0), mlr, cluster, ggplot2, gridExtra\n",
    sep=""
  )
  sink(NULL)
}
