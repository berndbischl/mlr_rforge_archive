source("defs.R")
source("getResampling.R")

ds = "<%=ds%>"
learner.id = "<%=learner.id%>"
split = <%=split%>
split.name = "<%=split.name%>" 
measure.id = "<%=measure.id%>" 

message("Loading dataset: ", ds)
e = getDataset(ds, attach=FALSE)
rin = getResampling(ds, split, split.name)
measure = measures[[measure.id]]
learner = learners.classif[[learner.id]]
message("Resampling: ", e$task@desc@id, ",", learner@id, ",", split.name, ",", measure@id)
#result = bench.exp(learner, e$task, rin, measures=ms)
result = resample(learner, e$task, rin, measures=measure)
fn = makeFileName(results.path, "result", ds, learner, split.name, measure, "RData")
message("Saving: ", fn)
# todo: delete other measures if no tuning was done
save(result, file=fn)        
