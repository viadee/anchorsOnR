library("anchors")
library("h2o")

data(iris)

h2o.init()

iris.data = as.h2o(iris, destination_frame = "iris.hex")

model = h2o.naiveBayes(x =1:4,
        y = 5,
        training_frame = iris.data)

pert = makePerturbFun(cl = "tabular.featureless")

discIris=iris
discIris = arules::discretizeDF(discIris)
explainer = anchors(iris, model, pert, target = 5, discX=discIris)

explanations = explain(iris[sample(nrow(iris), 3), ], explainer)

printExplanations(explainer, explanations)

