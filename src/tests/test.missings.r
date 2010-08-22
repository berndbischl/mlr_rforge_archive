library(mlr)
library(mlbench)
data(Sonar)
df =Sonar
df[1:5,1:2] = NA
ct = make.task(data=df, target="Class")

wls = get.learners("classif", missings=TRUE)
for (wl in wls) {
	print(wl)
	m=train(wl, ct)
	p = predict(m, ct)
	if(any(is.na(p["response"])))
		print("NA!")
}

