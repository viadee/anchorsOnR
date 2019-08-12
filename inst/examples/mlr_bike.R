library(anchors)
library(mlr)

set.seed(1)

load("inst/examples/bike.RData")

bike$target = factor(resid(lm(cnt ~ days_since_2011, data = bike)) > 0, levels = c(FALSE, TRUE), labels = c('below', 'above'))
#bike$target = arules::discretize(bike$cnt, labels = c("low", "low-medium", "medium", "medium-high", "high"), breaks = 5)
bike$cnt = NULL

bike.task = makeClassifTask(data = bike, target = "target")
#mod = mlr::train(mlr::makeLearner(cl = 'classif.rpart', id = 'bike-rf'), bike.task)
#rpart.plot::rpart.plot(getLearnerModel(mod))
mod = mlr::train(mlr::makeLearner(cl = 'classif.randomForest', id = 'bike-rf'), bike.task)

prediction = predict(mod$learner.model, bike[, !names(bike) %in% "target"], type = "class")
#length(which(prediction == bike$target)) / length(prediction)


explainer = anchors(bike, mod, target = "target", tau = 0.8, batchSize = 1000, maxAnchors = 5)#, bins = bins)

explained.instances = bike[sample(1:nrow(bike), 10),]
explanation = explain(explained.instances, explainer)

printExplanations(explainer, explanation)
