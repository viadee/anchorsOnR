perturbate <- function(perturbationFun, dataset, bins, instance, anchors, probKeep)
{
  # Directs to the perturbation function, function name is perturbate.[perturbationFun[[1]]], for instance, perturbate.tabular.featurelessDisc
  # tabular.featurelessDisc builds instances with bin-values while tabular.featureless builds instances with actual feature values
  UseMethod("perturbate")
}
