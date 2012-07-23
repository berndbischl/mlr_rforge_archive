library("devtools")
library("testthat")
library(mlbench)
data(BostonHousing)

load_all("skel")

task = makeClassifTask(data=iris, target="Species")

#lrn1 = makeLearner("classif.rpart", minsplit=3)
z = filterFeatures(task)
print(z)

#mod = train(lrn2, task)

#print(getHyperPars(lrn2))
#lrn2 = setHyperPars(lrn2, minsplit=12)
#print(getHyperPars(lrn2))


