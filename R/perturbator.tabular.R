#' @title
#' Tabular perturbator
#'
#' @description
#' Perturbation function for tabular data
#'
#' @rdname perturbTabularDisc
#' @export
perturbTabular = makePerturbator(
  perturbator = function(dataset, bins, instance, anchors, p, ...) {
    pertCols <- setdiff(seq(1, ncol(dataset), 1), anchors)

    # Perturbate columns that are not anchors
    for (i in pertCols) {
      instance <- exchangeFeatureValue(dataset, instance, i, p)
    }

    # Perturbate anchor-values within the respective bin (only if bins exist - discretizatin disabled?)
    for (i in anchors) {
      if (is.null(bins[[i]]$doDiscretize) || bins[[i]]$doDiscretize) {
        relevantBin <- provideBin(instance[, i], bins[[i]])
        matchingRows <- sapply(dataset[, i], checkBin, bins[[i]], relevantBin)
        instance <- exchangeFeatureValue(dataset[matchingRows, ], instance, i, p)
      }
    }

    return(instance)
  },
  supported = "tabular"
)

exchangeFeatureValue <- function(dataset, instance, feature, p) {
  if (as.logical(rbinom(1, size = 1, prob = 1 - p))) {
    instance[, feature] <- dataset[sample(rownames(dataset), 1), feature]
  }
  return(instance)
}
