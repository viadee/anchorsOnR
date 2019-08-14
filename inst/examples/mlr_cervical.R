library(anchors)
library(mlr)

set.seed(1)

load("inst/examples/cervical.RData")

cervical.task = makeClassifTask(data = cervical, target = "Biopsy")
mod = mlr::train(mlr::makeLearner(cl = 'classif.randomForest', id = 'cervical-rf',
                                  predict.type = 'prob'), cervical.task)

# Configure Discretization
bins = list(
  # Age
  c(19, 23, 28, 34),
  # Number.of.sexual.partners
  c(2,5,8),
  # First.sexual.intercourse
  c(13, 15, 17, 18, 25),
  # Num.of.pregnancies
  c(0, 1, 2, 4),
  # Smokes
  integer(),
  # Smokes..years.
  c(0, 2, 5, 10),
  # Hormonal.Contraceptives
  integer(),
  # Hormonal.Contraceptives..years.
  c(0, 1.672559, 5.403571, 11.564286, 20),
  # IUD
  integer(),
  # IUD..years.
  c(0, 1.588384, 5.208333, 9.125, 11),
  # STDs
  integer(),
  # STDs..number.
  c(0),
  # STDs..Number.of.diagnosis
  c(0),
  # STDs..Time.since.first.diagnosis
  c(0),
  # STDs..Time.since.last.diagnosis
  c(0)
)

# Explain model with anchors
explainer = anchors(cervical, mod, tau = 0.5, bins = bins)

explanation = explain(cervical[c(1,7),], explainer, labels = c("Healthy", "Cancer")) #326

# Print explanations
printExplanations(explainer, explanation)

plotExplanations(explanation)
