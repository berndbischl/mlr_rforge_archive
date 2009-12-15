# Test set.train.par with LDA-prior

test.set.par <- function() {
	ct <- make.classif.task(data=iris, target="Species")
	wl <- new("lda")
	wl <- set.train.par(wl, prior=c(0.4,0.6,0))
	m1 <- train(wl, ct)
	m2 <- lda(Species~., data=iris, prior = c(0.4,0.6,0))
	checkEqualsNumeric(m1["learner.model"]$prior, m2$prior)
}






