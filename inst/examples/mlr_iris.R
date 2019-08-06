library(anchors)
library(mlr)

set.seed(123)

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
bins = buildBins(columnIndex = 1, cuts = c(4.3, 5.4, 6.3, 7.9))
bins = buildBins(columnIndex = 2, cuts = c(2.0, 2.9, 3.2, 4.4), currentBins = bins)
bins = buildBins(columnIndex = 3, cuts = c(1, 2.633333, 4.9, 6.9), currentBins = bins)
bins = buildBins(columnIndex = 4, cuts = c(0.1, 0.8666667, 1.6, 2.5), currentBins = bins)

# Prepare explainer to explain model with anchors
explainer = anchors(iris, model, bins = bins, target = "Species")

# Produce explanations for selected instances
toExplain = iris[sample(1:nrow(iris), 10),]

explanations = explain(toExplain, explainer)

# print explanations
printExplanations(explainer, explanations)

plotExplanations(explanations)

