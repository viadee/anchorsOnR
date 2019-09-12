#' @title
#' Tabular perturbator
#'
#' @description
#' Perturbation function for tabular data
#'
#' @param dataset the dataset from which perturbations will be drawn
#' @param discretized_dataset the discretized dataset
#' @param instance the instance to be perturbed
#' @param discretized_instance the discretized version of the instance
#' @param anchors selected anchors in the data set
#' @param p possibility of an a feature to be perturbated. Anchors are not perturbated.
#' @param ... further arguments to be passed
#' @return perturbated instance
#' @family pertubators
#' @export
perturbTabularDisc = makePerturbator(
  perturbator = function(dataset, discretized_dataset, instance, discretized_instance, anchors, p, ...) {

    # Fetch one other random instance whose values get copied randomly
    sampled_instance <- discretized_dataset[sample(rownames(discretized_dataset), 1), ]

    # Determine columns to be perturbed
    pertCols <- setdiff(seq(1, ncol(dataset), 1), anchors)

    for (i in pertCols) {
      # If the value is not to be changed. Should not occur in this function anyways, as usually p = 1
      if (as.logical(stats::rbinom(1, size = 1, prob = p)) == F) {
        instance[, i] <- discretized_instance[, i]
      } else {
        # If the value is to be changed take a random other class and its discretized value
        instance[, i] <- sampled_instance[, i]
      }
    }

    return(instance)
  },
  supported = "tabular"
)
