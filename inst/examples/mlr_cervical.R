library(anchors)
library(mlr)

set.seed(123)

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

# Build bins by using helper function
# Age
bins=buildBins(columnIndex=1, cuts=c(15, 25, 35, 50, 60), currentBins = NULL)
# Number.of.sexual.partners
bins=buildBins(columnIndex=2, cuts=c(1,2,8),
               #arules::discretize(cervical[, 2], breaks = 2, onlycuts = T),
               currentBins=bins)
# First.sexual.intercourse
bins=buildBins(columnIndex=3, cuts=c(11, 15, 17, 18, 29),
                 #arules::discretize(cervical[, 3], breaks = 4, onlycuts = T),
                 currentBins=bins)
# Num.of.pregnancies
bins=buildBins(columnIndex=4, cuts=c(0, 1, 2, 4), currentBins=bins)
# Smokes
bins=buildBins(columnIndex=5, disc=F, currentBins=bins)
# Smokes..years.
bins=buildBins(columnIndex=6, cuts=c(0, 2, 5, 10), currentBins=bins)
# Hormonal.Contraceptives
bins=buildBins(columnIndex=7, disc=F, currentBins=bins)
# Hormonal.Contraceptives..years.
bins=buildBins(columnIndex=8, cuts=c(0, 1.672559, 5.403571, 11.564286, 20),
                 #arules::discretize(cervical[, 8], method = "cluster", breaks = 4, onlycuts = T),
               currentBins=bins)
# IUD
bins=buildBins(columnIndex=9, disc=F, currentBins=bins)
# IUD..years.
bins=buildBins(columnIndex=10, cuts=c(0, 1.588384, 5.208333, 9.125, 11),
                 #arules::discretize(cervical[, 10], method = "cluster", breaks = 4, onlycuts = T),
               currentBins=bins)
# STDs
bins=buildBins(columnIndex=11, disc=F, currentBins=bins)
# STDs..number.
bins=buildBins(columnIndex=12, cuts=c(1), currentBins=bins)
# STDs..Number.of.diagnosis
bins=buildBins(columnIndex=13, cuts=c(1), currentBins=bins)
# STDs..Time.since.first.diagnosis
bins=buildBins(columnIndex=14, cuts=c(3), currentBins=bins)
# STDs..Time.since.last.diagnosis
bins=buildBins(columnIndex=15, cuts=c(3), currentBins=bins)


# Explain model with anchors
explainer = anchors(cervical, model, bins = bins)

explanations = explain(cervical[1:10,], explainer)

printExplanations(explainer, explanations)

plotExplanations(explanations)
