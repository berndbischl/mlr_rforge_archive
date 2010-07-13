source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="debug")
parallel.setup(mode="local")


data(BostonHousing)

rt = make.task(data=BostonHousing, target="medv")

m = train("regr.earth", rt)
p = predict(m, rt)
print(performance(p))

