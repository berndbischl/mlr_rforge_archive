library(mlbench)
data(BreastCancer)
breast <- na.omit(BreastCancer[-c(1,11)])
breast_label <- na.omit(BreastCancer[,11])