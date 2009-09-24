# Test set.train.par with LDA-prior

library(MASS)
train.inds <- seq(1,150,2)
test.inds <- seq(2,150,2)

ct <- make.classif.task("lda", data=iris, formula=Species~.)
ct2 <- set.train.par(ct, prior=c(0.4,0.6,0))
cm <- train(ct2, subset=train.inds)


m <- lda(Species~., data=iris, prior = c(0.4,0.6,0))

cm["learner.model"]$prior == m$prior
#setosa versicolor  virginica 
#TRUE       TRUE       TRUE 






