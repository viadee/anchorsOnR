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
      if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
        # Basically to featureless perturbation for this column
        if (as.logical(rbinom(1, size = 1, prob = probKeep))) {
          instance[, i] = dataset[sample(rownames(dataset), 1), i]
        }
        next
      }
      # Check bin of instance
      binNr = provideBin.numeric(instance[i], bin$cuts, bin$right)
      binsNo = 1:(length(bins[[i]]$cuts) + 1)

      instance[, i] = sample(c(binNr, binsNo[-binNr]), 1,
                             p = c(probKeep, rep((1 - probKeep) / length(bin$cuts),
                                                 length(bin$cuts)
                             )))
    }

    return(instance)
  }
