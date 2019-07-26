library("h2o")

data(iris)

h2o.init()

iris.data = as.h2o(iris, destination_frame = "iris.hex")

model = h2o.naiveBayes(x =1:4,
        y = 5,
        training_frame = iris.data)

pert = makePerturbFun(cl = "tabular.featureless")

explainer = anchors(iris, model, pert, target = 5)

explanations = explain(iris[1:2,], explainer)

printExplanations(explainer, explanations)

plotExplanations(explanations)
