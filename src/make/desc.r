

write.desc <- function(desc.path, rev.nr) {

	sink(file=desc.path)
	
	cat(
		"Package: mlr\n",
		"Type: Package\n",
		"Title: mlr: Machine Learning in R\n",
		paste("Version: 0.2.", rev.nr, "\n", sep=""),
		"Date: 2009-01-01\n",
		"Author: Bernd Bischl, Max Wornowizki, Katharina Borg\n",
		"Maintainer: Bernd Bischl <bernd_bischl@gmx.net>\n",
		"Description: no\n",
		"License: GPL (>= 2)\n",
		"LazyLoad: yes\n",
		"Depends: R (>= 2.8.0), utils, methods, reshape, roxygen, stats, boot, e1071, klaR\n",
		"Suggests: MASS, kernlab, rpart, randomForest, adabag, kknn, snowfall\n",
		sep=""
	)
	
	sink(NULL)

}