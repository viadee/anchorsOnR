#' @title Perturbate an instance
#'
#' @param perturbationFun defines how the instances is perturbated. Usually, you
#' will use \code{\link{makeTabularPerturbator}} to create a perturbation operator
#' @param dataset the dataset from which perturbations will be drawn
#' @param discretized_dataset the discretized dataset
#' @param instance the instance to be perturbed
#' @param discretized_instance the discretized version of the instance
#' @param anchors indizes of features that will not be perturbated
#' @param p possibility of an a feature to be perturbated. Anchors are not perturbated.
#'
#' @return perturbated instance
#' @export
#'
perturbate <- function(perturbationFun, dataset, discretized_dataset, instance, discretized_instance, anchors, p)
{
 do.call(perturbationFun, list(dataset, discretized_dataset, instance, discretized_instance, anchors, p))
}
