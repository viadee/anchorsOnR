#' @title
#' Tabular perturbator
#'
#' @description
#' Perturbation function for tabular data
#'
#' @param perturbationFun defines how the instances is perturbated. Usually, you
#' will use \code{\link{makeTabularPerturbator}} to create a perturbation operator
#' @param dataset the dataset from which perturbations will be drawn
#' @param bins the bins of the discretized dataset
#' @param instance the instance to be perturbated
#' @param anchors indizes of features that will not be perturbated
#' @param p possibility of an a feature to be perturbated. Anchors are not perturbated.
#' @return perturbated instance
#' @family pertubators
#' @export
perturbTabularDisc = makePerturbator(
  perturbator = function(dataset, bins, instance, anchors, p, ...) {
    pertCols <- setdiff(seq(1, ncol(dataset), 1), anchors)

    for (i in pertCols) {
      bin <- bins[[i]]
      # Check if discretization has been disabled
      # We might be able to remove this check as line 35 works equally
      if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
        # Basically to featureless perturbation for this column
        instance <- exchangeFeatureValue(dataset, instance, i, p)
        next
      }
      # Check bin of instance
      providedBin <- provideBin(instance[i], bin)
      binsNo <- 1:(length(bins[[i]]$cuts) + 1)

      instance[, i] <- sample(c(providedBin, binsNo[-providedBin]), 1,
                              p = c(p, rep(
                                (1 - p) / length(bin$cuts),
                                length(bin$cuts)
                              ))
      )
    }

    return(instance)
  },
  supported = "tabular"
)
