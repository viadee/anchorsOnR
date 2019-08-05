makeRPerturbFun.tabular.featureless <- function() {
  makeRPerturbFunTabular(
    cl = "tabular.featureless",
    package = "stringr",
    name = "Featureless Perturbation",
    short.name = "fl",
    note = ""
  )
}

makeRPerturbFun.tabular.featurelessDisc <- function() {
  makeRPerturbFunTabular(
    cl = "tabular.featurelessDisc",
    package = "stringr",
    name = "Featureless Perturbation",
    short.name = "fld",
    note = ""
  )
}

perturbate.tabular.featureless <-
  function(perturbFun,
           dataset,
           bins,
           instance,
           anchors,
           probKeep,
           ...) {
    pertCols = setdiff(seq(1, ncol(dataset), 1), anchors)

    # Perturbate columns that are not anchors
    for (i in pertCols) {
      instance=perturbate.tabular.exchangeFeatureValue(dataset, instance, i, probKeep)

    }

    # Perturbate anchor-values within the respective bin (only if bins exist - discretizatin disabled?)
        for (i in anchors) {

          if (is.null(bins[[i]]$doDiscretize) || bins[[i]]$doDiscretize) {
          relevantBin=provideBin(instance[,i], bins[[i]])
          matchingRows=sapply(dataset[,i], checkBin, bins[[i]], relevantBin)
          instance=perturbate.tabular.exchangeFeatureValue(dataset[matchingRows,], instance, i, probKeep)
          }
        }


    return(instance)
  }


perturbate.tabular.featurelessDisc <-
  function(perturbFun,
           dataset,
           bins,
           instance,
           anchors,
           probKeep,
           ...) {

    pertCols = setdiff(seq(1, ncol(dataset), 1), anchors)

    for (i in pertCols) {
      bin <- bins[[i]]
      # Check if discretization has been disabled
      # We might be able to remove this check as line 35 works equally
      if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
        # Basically to featureless perturbation for this column
        instance=perturbate.tabular.exchangeFeatureValue(dataset, instance, i, probKeep)
        next
      }
      # Check bin of instance
      providedBin = provideBin(instance[i], bin)
      binsNo = 1:(length(bins[[i]]$cuts) + 1)

      instance[, i] = sample(c(providedBin, binsNo[-providedBin]), 1,
                             p = c(probKeep, rep((1 - probKeep) / length(bin$cuts),
                                                 length(bin$cuts)
                             )))
    }

    return(instance)
  }

perturbate.tabular.exchangeFeatureValue <- function(dataset, instance, feature, probKeep){

  if (as.logical(rbinom(1, size = 1, prob = 1-probKeep))) {
    instance[, feature] = dataset[sample(rownames(dataset), 1), feature]
  }
  return(instance)

}
