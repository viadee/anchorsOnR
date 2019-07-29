makePerturbationTask <- function(df = NULL, instance = NULL){

}

#' @title makePerturbFun
#' @name makePerturbFun
#' @rdname makePerturbFun
#'
#' @export
makePerturbFun <- function(cl, id = cl, ..., par.vals = list(), config = list()){
  checkmate::assertString(cl)
  checkmate::assertList(config, names = "named")
  if ("show.info" %in% names(config))
    stop("'show.info' cannot be set in 'makePerturbFun', please use 'configureAnchors' instead.")
  #assertSubset(names(config), choices = names(getAnchorsOptions()))
  constructor = try(getS3method("makeRPerturbFun", class = cl),
                    silent = TRUE)
  if (inherits(constructor, "try-error")) {
    stopf("Couldn't find perturbation function '%s'")
    #  possibles = getNameProposals(cl, possible.inputs = suppressWarnings(listPerturbationFuns()$class))
  #  stopf("Couldn't find perturbation function '%s'\nDid you mean one of these perturbation functions instead: %s",
  #        cl, stri_flatten(possibles, collapse = " "))
  }
  wl = do.call(constructor, list())
  wl$config = config
  if (!missing(id)) {
    checkmate::assertString(id)
    wl$id = id
  }
 # assertList(par.vals, names = "unique")
  if (stringi::stri_isempty(cl))
    stop("Cannot create perturbation function from empty string!")
  if (!inherits(wl, "RPerturbFun"))
    stop("Perturbation Function must be a basic PerturbFun!")
#  wl = setHyperPars(learner = wl, ..., par.vals = par.vals)
#  wl = setPredictType(learner = wl, predict.type = predict.type)
#  if (!is.null(predict.threshold))
 #   wl = setPredictThreshold(wl, predict.threshold)
#  wl$fix.factors.prediction = fix.factors.prediction
  return(wl)
}

# temp helpers
getAnchorsOptions = function(){
  return(getMlrOptions())
}

listPerturbationFuns = function(){
  return(listLearners())
}
