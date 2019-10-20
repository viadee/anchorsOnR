library(anchors)
library(h2o)

set.seed(123)

data(iris)

h2o.init()

model = h2o.naiveBayes(x = 1:4,
        y = 5,
        training_frame = as.h2o(iris, destination_frame = "iris.hex"))

# Prepare bins:
# a list of bins per column, the nth-entry provides the bins of the nth-column
# Per bin: define whether numeric, the cuts
# of the bins, and whether the right or the left border of a bin is included
bins = list()
bins[[1]] = list(cuts = c(4.3, 5.4, 6.3, 7.9))
bins[[2]] = list(cuts = c(2.0, 2.9, 3.2, 4.4))
bins[[3]] = list(cuts = c(1, 2.633333, 4.9, 6.9))
bins[[4]] = list(cuts = c(0.1, 0.8666667, 1.6, 2.5))

# Prepare explainer to explain model with anchors
explainer = anchors(iris, model, target = "Species", bins = bins, tau = 1)

explanations = explain(iris[sample(nrow(iris), 3), ], explainer)

printExplanations(explainer, explanations)

h2o.shutdown(prompt = FALSE)

plotExplanations(explanations)

