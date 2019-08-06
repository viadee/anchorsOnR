library(anchors)
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
# Per bin: define whether numeric, the cuts
# of the bins, and whether the right or the left border of a bin is included
bins=buildBins(columnIndex=1, arules::discretize(iris[, 1], onlycuts = T), currentBins = NULL)
r=lapply(2:(ncol(iris) - 1), function(i) {
  bins<<-buildBins(columnIndex=i, arules::discretize(iris[, i], onlycuts = T), currentBins = bins)
})

# Prepare explainer to explain model with anchors
explainer = anchors(iris, model, bins = bins, target = "Species")

# Produce explanations for selected instances
toExplain = iris[sample(1:nrow(iris), 1),]

explanations = explain(toExplain, explainer)

# print explanations
printExplanations(explainer, explanations)

plotExplanations(explanations)

