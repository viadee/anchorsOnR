makeRPerturbFun <- function(){
  UseMethod("makeRPerturbFun")
}

makeRPerturbFunTabular = function (cl, package, name = cl, short.name = cl, note = "") {
  perturbFun = BBmisc::addClasses(makeRPerturbFunInternal(id = cl, type = "tabular", package = package,
                                        name = name, short.name = short.name, note = note), c(cl, "RPerturbFunTabular"))

  return(perturbFun)
}

makeRPerturbFunInternal = function(id, type, package,
                                name = id, short.name = id, note = ""
                                ) {

  checkmate::assertCharacter(package, any.missing = FALSE)
  BBmisc::requirePackages(package, why = stri_paste("perturbation function", id, sep = " "), default.method = "load")

  checkmate::assertString(id)
  checkmate::assertChoice(type, choices = c("tabular", "text", "image", "audio", "video"))
  checkmate::assertString(name)
  checkmate::assertString(short.name)
  checkmate::assertString(note)
  perturbFun = makePerturbFunBaseConstructor("RPerturbFun",
                                       id = id,
                                       type = type,
                                       package = package
  )
  perturbFun$name = name
  perturbFun$short.name = short.name
  perturbFun$note = note
  return(perturbFun)

}
