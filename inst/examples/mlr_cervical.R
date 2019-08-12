library(anchors)
library(mlr)

set.seed(1)

load("inst/examples/cervical.RData")

cervical.task = makeClassifTask(data = cervical, target = "Biopsy")
mod = mlr::train(mlr::makeLearner(cl = 'classif.randomForest', id = 'cervical-rf', predict.type = 'prob'), cervical.task)

# library(iml)
# library(ggplot2)
# pred.cervical = Predictor$new(mod, data = cervical, class = "Cancer")
# pdp = FeatureEffect$new(pred.cervical, "Age", method = "pdp")
# p1 = pdp$plot() +
#   scale_x_continuous(limits = c(0, NA)) +
#   scale_y_continuous('Predicted cancer probability', limits = c(0, 0.4))
# pdp$set.feature("Hormonal.Contraceptives..years.")
# p2 = pdp$plot() +
#   scale_x_continuous("Years on hormonal contraceptives", limits = c(0, NA)) +
#   scale_y_continuous('', limits = c(0, 0.4))
# gridExtra::grid.arrange(p1, p2, ncol = 2)



# discretize <- function(x, breaks) {
#   setdiff(arules::discretize(x, onlycuts = T, infinity = T, breaks = breaks), c(Inf, -Inf))
# }
#
# discretize.cluster <- function(x, breaks) {
#   setdiff(arules::discretize(x, onlycuts = T, infinity = T, method = "cluster", breaks = breaks), c(Inf, -Inf))
# }

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

#smoted_cervical <- DMwR::SMOTE(Biopsy ~ ., cervical, perc.over = 500, perc.under = 150)
#table(smoted_cervical$Biopsy)

# Explain model with anchors
explainer = anchors(cervical, mod, tau = 0.5)#, bins = bins)

# Create explanations
#library(profvis)

#profvis({
explanation = explain(cervical[1,], explainer, labels = "Healthy") #326
#})

# Print explanations
printExplanations(explainer, explanation)
