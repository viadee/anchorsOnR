library(mlr)

load("inst/examples/ExampleCancer/cervical.RData")

cervical_label_cancer = cervical[cervical$Biopsy == "Cancer",]
cervical_label_healthy = cervical[cervical $Biopsy == "Healthy",]
cervical_label_healthy = cervical_label_healthy[sample(1:nrow(cervical_label_healthy), nrow(cervical_label_cancer)), ]
cervical = rbind(cervical_label_cancer, cervical_label_healthy)

cervical.task = makeClassifTask(data = cervical, target = "Biopsy")
model = mlr::train(mlr::makeLearner(cl = 'classif.rpart', id = 'cervical-rf', predict.type = 'prob'), cervical.task)

# Visualize
rpart.plot::rpart.plot(getLearnerModel(model))

bins <- list()
for (i in 1:(ncol(cervical)-1)) {
  bins[[i]] <- list()
  bins[[i]]$doDiscretize <- T
  #bins[[i]]$numeric <- T
  #bins[[i]]$right =
}

# Age
bins[[1]]$cuts <- c(15, 25, 35, 50, 60)
# Number.of.sexual.partners
bins[[2]]$cuts <- arules::discretize(cervical[, 2], breaks = 2, onlycuts = T)
# First.sexual.intercourse
bins[[3]]$cuts <- arules::discretize(cervical[, 3], breaks = 4, onlycuts = T)
# Num.of.pregnancies
bins[[4]]$cuts <- c(0, 1, 2, 4)
# Smokes
bins[[5]]$doDiscretize <- F
# Smokes..years.
bins[[6]]$cuts <- c(0, 2, 5, 10)
# Hormonal.Contraceptives
bins[[7]]$doDiscretize <- F
# Hormonal.Contraceptives..years.
bins[[8]]$cuts <- arules::discretize(cervical[, 8], method = "cluster", breaks = 4, onlycuts = T)
# IUD
bins[[9]]$doDiscretize <- F
# IUD..years.
bins[[10]]$cuts <- arules::discretize(cervical[, 10], method = "cluster", breaks = 4, onlycuts = T)
# STDs
bins[[11]]$doDiscretize <- F
# STDs..number.
bins[[12]]$cuts <- c(0, 1)
# STDs..Number.of.diagnosis
bins[[13]]$cuts <- c(0, 1)
# STDs..Time.since.first.diagnosis
bins[[14]]$cuts <- c(0, 3)
# STDs..Time.since.last.diagnosis
bins[[15]]$cuts <- c(0, 3)


# Explain model with anchors
explainer = anchors(cervical, model, bins = bins)

explanations = explain(cervical[3,], explainer)

printExplanations(explainer, explanations)

