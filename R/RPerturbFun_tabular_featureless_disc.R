makeRPerturbFun.tabular.featurelessDisc <- function(){
  makeRPerturbFunTabular(
    cl = "tabular.featurelessDisc",
    package = "stringr",
    # properties = c("factors, text, numerical"),
    name = "Featureless Perturbation",
    short.name = "fld",
    note = ""#,
    #callees = character(0)
  )
}

perturbate.tabular.featurelessDisc <- function(perturbFun, dataset, datasetDisc, instance, anchors, ...){

  pertCols = setdiff(seq(1, ncol(datasetDisc), 1), anchors)

  for(i in pertCols){

    lvls = levels(datasetDisc[,i])

    lvl = which(sapply(lvls, function(x){
      if(stringr::str_detect(x,"[(\\[]\\d+\\.?(\\d+)?,\\d+\\.?(\\d+)?[)\\]]")){
        return(isInIntervall(x, as.numeric(instance[i])))
      } else {
        return(x == instance[i])
      }
    }))

    lvl = names(lvl)
    instance[,i] = sample(c(lvl,lvls[-match(lvl,lvls)]), 1, p=c(0.5,rep((1-0.5)/(length(lvls)-1),length(lvls)-1)))
    #group = which(datasetDisc[,i] != lvls[lvl])

    #instance[,i] = datasetDisc[sample(datasetDisc, 1), i]
  }

  return(instance)
}
