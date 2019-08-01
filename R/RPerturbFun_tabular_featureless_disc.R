makeRPerturbFun.tabular.featurelessDisc <- function() {
  makeRPerturbFunTabular(
    cl = "tabular.featurelessDisc",
    package = "stringr",
    name = "Featureless Perturbation",
    short.name = "fld",
    note = ""
  )
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
        if (as.logical(rbinom(1, size = 1, prob = probKeep))) {
          instance[, i] = dataset[sample(rownames(dataset), 1), i]
        }
        next
      }
      # Check bin of instance
      providedBin = provideBin.numeric(instance[i], bin)
      binsNo = 1:(length(bins[[i]]$cuts) + 1)

      instance[, i] = sample(c(providedBin, binsNo[-providedBin]), 1,
                             p = c(probKeep, rep((1 - probKeep) / length(bin$cuts),
                                                 length(bin$cuts)
                             )))
    }

    return(instance)
  }
