checkPerturbFun = function(perturbFun, type = NULL, props = NULL) {
  if (is.character(perturbFun))
    perturbFun = makePerturbFun(perturbFun)
  else
    checkmate::assertClass(perturbFun, classes = "PerturbFun")

  if (!is.null(type) && perturbFun$type %nin% type) {
    stopf("PerturbFun '%s' must be of type '%s', not: '%s'", perturbFun$id, collapse(type), perturbFun$type)
  }

  if (!is.null(props)) {
    perturbFun.props = getPerturbFunProperties(perturbFun)
    missing.props = setdiff(props, perturbFun.props)
    if (length(missing.props) > 0L){
      stopf("PerturbFun '%s' must support properties '%s', but does not support '%s'.", perturbFun$id, collapse(props), collapse(missing.props))
    }
  }

  return(perturbFun)
}
