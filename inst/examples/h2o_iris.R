library(anchors)
library(h2o)

set.seed(123)

data(iris)

h2o.init()

model = h2o.naiveBayes(x = 1:4,
        y = 5,
        training_frame = as.h2o(iris, destination_frame = "iris.hex"))

explainer = anchors(iris, model, target = "Species")

explanations = explain(iris[sample(nrow(iris), 3), ], explainer)

printExplanations(explainer, explanations)

