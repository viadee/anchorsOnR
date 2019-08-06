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
  constructor = try(getS3method("makeRPerturbFun", class = cl),
                    silent = TRUE)
  if (inherits(constructor, "try-error")) {
    stopf("Couldn't find perturbation function '%s'")
  }

  wl = do.call(constructor, list())
  wl$config = config
  if (!missing(id)) {
    checkmate::assertString(id)
    wl$id = id
  }
  if (stringi::stri_isempty(cl))
    stop("Cannot create perturbation function from empty string!")
  if (!inherits(wl, "RPerturbFun"))
    stop("Perturbation Function must be a basic PerturbFun!")
  return(wl)
}
