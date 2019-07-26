makePerturbFunBaseConstructor <- function(classes, id, type, package#,
                                         #properties, par.set, par.vals, predict.type, cache = FALSE
                                         ) {

  # if (length(par.vals) == 0L) {
  #   names(par.vals) = character(0L)
  # }

  perturbFun = BBmisc::makeS3Obj(c(classes, "perturbFun"),
                      id = id,
                      type = type,
                      package = package#,
                      # properties = unique(properties),
                      # par.set = par.set,
                      # par.vals = par.vals,
                      # predict.type = predict.type,
                      # cache = cache
  )
  return(perturbFun)
}

#' @export
print.PerturbFun = function(x, ...) {

  cat(
    "Perturbation Function ", x$id, " from package ", collapse(cleanupPackageNames(x$package)), "\n",
    "Type: ", x$type, "\n",
    "Name: ", x$name, "; Short name: ", x$short.name, "\n",
    "Class: ", class(x)[1L], "\n",
  #  "Properties: ", collapse(getLearnerProperties(x)), "\n",
  #  "Predict-Type: ", x$predict.type, "\n",
  #  "Hyperparameters: ", getHyperParsString(x, show.missing.values = TRUE), "\n\n",
    sep = ""
  )
}
