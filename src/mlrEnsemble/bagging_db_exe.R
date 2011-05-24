# TODO: Add comment
# 
# Author: ollishellshock
###############################################################################


source("/home/meyer/HiWi/bagging/src/bagging_db_scr.R")

df1 = iris[seq(1,150,2),]
df2 = iris[seq(2,150,2),]

class = "classif.lda"
parts = 4
rep = 1
cpu = 1
target = "Species"

library(snowfall)
library(mlr)

resp.ave <- dist.db.bagging(df1=df1, df2=df2, class=class, target=target, cpu=cpu, parts=parts, rep=rep)
