#' @title
#' Tabular perturbator
#'
#' @description
#' Perturbation function for tabular data
#'
#' @rdname perturbTabularDisc
#' @export
perturbTabular = makePerturbator(
  perturbator = function(dataset, discretized_dataset, instance, discretized_instance, anchors, p, ...) {

    # Fetch one other random instance whose values get copied randomly
    sampled_instance <- dataset[sample(rownames(dataset), 1), ]

    # Determine columns to be perturbed
    pertCols <- setdiff(seq(1, ncol(dataset), 1), anchors)

    # Perturbate columns that are not anchors
    for (i in pertCols) {
      if (as.logical(stats::rbinom(1, size = 1, prob = p))) {
        instance[, i] <- sampled_instance[, i]
      }
    }

    # Perturbate anchor-values within the respective bin (only if bins exist - discretizatin disabled?)
    for (i in anchors) {
      matching_disc_rows <- which(discretized_dataset[, i] == discretized_instance[, i])
      # If length == 0 -> No other disc found, keep current value
      # If length == 1 -> only one instance (the original one) is applicable.
      # sample will work differently when called with numeric vector of length one
      if (length(matching_disc_rows) == 0) {
        next
      } else if (length(matching_disc_rows) == 1) {
        sampled_disc_row <- matching_disc_rows
      } else {
        sampled_disc_row <- sample(matching_disc_rows, 1)
      }
      instance[, i] <- dataset[sampled_disc_row, i]
    }

    return(instance)
  },
  supported = "tabular"
)
