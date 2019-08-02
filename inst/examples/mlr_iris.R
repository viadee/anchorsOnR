library("anchors")
library(mlr)

data(iris)

# our goal is to predict the species
task = makeClassifTask(data = iris, target = "Species", id = "iris")

# setting up a learner
lrn = makeLearner("classif.rpart")

# train the learner on the training set
model = mlr::train(learner = lrn, task = task)

# visualize
rpart.plot::rpart.plot(getLearnerModel(model))

# Prepare bins:
# a list of bins per column, the nth-entry provides the bins of the nth-column
# Per bin: define whether numeric (for now: only works with numeric), the cuts
# of the bins, and whether the right or the left border of a bin is included
bins = lapply(1:(ncol(iris) - 1), function(x) {
  bin <- list()
  cuts = arules::discretize(iris[, x], onlycuts = T)
  bin$cuts <- cuts[2:(length(cuts) - 1)]
  bin$right <- F
  return(bin)
})

# Prepare explainer to explain model with anchors
explainer = anchors(iris, model, bins = bins, target = "Species")

# Produce explanations for selected instances
toExplain = iris[sample(1:nrow(iris), 20),]
explanations = explain(toExplain, explainer)

# print explanations
printExplanations(explainer, explanations)

