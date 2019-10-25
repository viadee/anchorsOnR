library(MASS)
data(iris)

set.seed(123)

model <- lda(Species~., data = iris)

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

# Construct the explanations
explanations = explain(iris[sample(nrow(iris),3),], explainer)

# print explanations
printExplanations(explainer, explanations)

plotExplanations(explanations)
