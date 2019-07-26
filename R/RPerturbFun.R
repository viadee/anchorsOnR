makeRPerturbFun <- function(){
  UseMethod("makeRPerturbFun")
}

makeRPerturbFunTabular = function (cl, package,
                                  #par.set, par.vals = list(), properties = character(0L),
          name = cl, short.name = cl, note = ""#, class.weights.param = NULL, callees = character(0L)
          ) {
  perturbFun = BBmisc::addClasses(makeRPerturbFunInternal(id = cl, type = "tabular", package = package,
                                        #par.set, par.vals, properties,callees
                                        name = name, short.name = short.name, note = note), c(cl, "RPerturbFunTabular"))
 # if ("class.weights" %in% getPerturbFunProperties(perturbFun)) {
 #    assertString(class.weights.param)
 #    if (!is.null(par.set$pars[[class.weights.param]]))
 #      lrn$class.weights.param = class.weights.param
 #    else stopf("'%s' needs to be defined in the parameter set as well.",
 #               class.weights.param)
 #  }
  return(perturbFun)
}

makeRPerturbFunInternal = function(id, type, package,
                                  #par.set, par.vals, properties,
                                name = id, short.name = id, note = ""#, callees
                                ) {

  checkmate::assertCharacter(package, any.missing = FALSE)
  BBmisc::requirePackages(package, why = stri_paste("perturbation function", id, sep = " "), default.method = "load")

  checkmate::assertString(id)
  checkmate::assertChoice(type, choices = c("tabular", "text", "image", "audio", "video"))
#  assertSubset(properties, listPerturbFunProperties(type))
#  assertClass(par.set, classes = "ParamSet")
#  checkListElementClass(par.set$pars, "LearnerParam")
#  assertList(par.vals)
#  if (!isProperlyNamed(par.vals))
#    stop("Argument par.vals must be a properly named list!")
  checkmate::assertString(name)
  checkmate::assertString(short.name)
  checkmate::assertString(note)
  #assertCharacter(callees, any.missing = FALSE)
  perturbFun = makePerturbFunBaseConstructor("RPerturbFun",
                                       id = id,
                                       type = type,
                                       package = package#,
                                      # properties = unique(properties),
                                      # par.set = par.set,
                                      # par.vals = par.vals,
                                      # predict.type = "response"
  )
  perturbFun$name = name
  perturbFun$short.name = short.name
  perturbFun$note = note
  #perturbFun$callees = callees
  #perturbFun$help.list = makeParamHelpList(callees, package, par.set)
  return(perturbFun)

}
