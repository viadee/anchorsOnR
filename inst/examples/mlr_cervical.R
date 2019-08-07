library(anchors)
library(mlr)

set.seed(1)

load("inst/examples/ExampleCancer/cervical.RData")

cervical_label_cancer = cervical[cervical$Biopsy == "Cancer",]
cervical_label_healthy = cervical[cervical $Biopsy == "Healthy",]
# Balance dataset by taking equal amount of observations of both classes
cervical_label_healthy = cervical_label_healthy[sample(1:nrow(cervical_label_healthy), nrow(cervical_label_cancer)), ]
cervical = rbind(cervical_label_cancer, cervical_label_healthy)

cervical.task = makeClassifTask(data = cervical, target = "Biopsy")
model = mlr::train(mlr::makeLearner(cl = 'classif.rpart', id = 'cervical-rf', predict.type = 'prob'), cervical.task)

# Visualize
rpart.plot::rpart.plot(getLearnerModel(model))

# Configure Discretization
bins = list(
  # Age
  c(15, 25, 35, 50, 60),
  # Number.of.sexual.partners
  c(1,2,8),
  # First.sexual.intercourse
  c(11, 15, 17, 18, 29),
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
  c(1),
  # STDs..Number.of.diagnosis
  c(1),
  # STDs..Time.since.first.diagnosis
  c(3),
  # STDs..Time.since.last.diagnosis
  c(3)
)

# Explain model with anchors
explainer = anchors(cervical, model, bins = bins)

# Create explanations
explanations = explain(cervical[1:1,], explainer)


printExplanations(explainer, explanations)

plotExplanations(explanations)
