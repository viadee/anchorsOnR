library(anchors)
library(mlr)

set.seed(1)

load("inst/examples/bike.RData")

bike$target = factor(resid(lm(cnt ~ days_since_2011, data = bike)) > 0,
                     levels = c(FALSE, TRUE), labels = c('below', 'above'))
bike$cnt = NULL

bike.task = makeClassifTask(data = bike, target = "target")
mod = mlr::train(mlr::makeLearner(cl = 'classif.randomForest',
                                  id = 'bike-rf'), bike.task)

bins = list(
  integer(),
  integer(),
  integer(),
  integer(),
  list(c("SAT", "SUN"), c("MON", "TUE"), c("WED", "THU", "FRI")),
  #integer(),
  integer(),
  integer(),
  # temp
  c(0, 7, 14, 21, 28),
  # hum
  c(30, 60, 69, 92),
  # windspeed
  c(5, 10, 15, 20, 25),
  integer()
)

explainer = anchors(bike, mod, target = "target", bins = bins, tau = 0.85, batchSize = 1000)

explained.instances = bike[sample(1:nrow(bike), 4),]
explanation = explain(explained.instances, explainer)

printExplanations(explainer, explanation)
plotExplanations(explanation)
