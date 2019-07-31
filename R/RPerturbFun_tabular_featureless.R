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

perturbate.tabular.featureless <- function(perturbFun, dataset, bins, instance, anchors, probKeep,...){

  pertCols = setdiff(seq(1, ncol(dataset), 1), anchors)

  for(i in pertCols){
    if (as.logical(rbinom(1,size = 1,prob = probKeep))){
      instance[,i] = dataset[sample(rownames(dataset), 1), i]
    }
  }

  return(instance)
}
