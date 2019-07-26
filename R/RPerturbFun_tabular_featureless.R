makeRPerturbFun.tabular.featureless <- function(){
  makeRPerturbFunTabular(
    cl = "tabular.featureless",
    package = "stringr",
    # properties = c("factors, text, numerical"),
    name = "Featureless Perturbation",
    short.name = "fl",
    note = ""#,
    #callees = character(0)
  )
}

perturbate.tabular.featureless <- function(perturbFun, dataset, datasetDisc, instance, anchors, ...){

  pertCols = setdiff(seq(1, ncol(datasetDisc), 1), anchors)

  for(i in pertCols){

    lvls = levels(datasetDisc[,i])

    lvl = which(sapply(lvls, function(x){
      if(stringr::str_detect(x,"[(\\[]\\d+\\.?(\\d+)?,\\d+\\.?(\\d+)?[)\\]]")){
        return(isInIntervall(x, instance[i]))
      } else {
        return(x == instance[i])
      }
    }))

    group = which(datasetDisc[,i] == lvls[lvl])

    instance[,i] = dataset[sample(group, 1), i]
  }
  return(instance)
}
