#' @title Get Properties of Pertubation Function
#' @rdname PerturbFunProperties
#' @export
getPerturbFunProperties = function(perturbFun) {
  UseMethod("getPerturbFunProperties")
}

#' @export
getPerturbFunProperties.PerturbFun = function(perturbFun) {
  perturbFun$properties
}

#' @export
getPerturbFunProperties.character = function(perturbFun) {
  getPerturbFunProperties(checkLearner(perturbFun))
}
