# Prepare data

library(mlbench)
data(BreastCancer)

# remove id
breast <- BreastCancer[,-1] 
# remove 14 cases
breast <- na.omit(breast)

