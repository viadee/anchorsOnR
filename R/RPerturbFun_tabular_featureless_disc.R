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

perturbate.tabular.featurelessDisc <- function(perturbFun, dataset, bins, instance, anchors, probKeep, ...){

  pertCols = setdiff(seq(1, ncol(dataset), 1), anchors)

  for(i in pertCols){

    # Check bin of instance
    bin = provideBin.numeric(instance[i], bins[[i]]$cuts, bins[[i]]$right)

    binsNo = 1:(length(bins[[i]]$cuts) + 1)

    instance[,i] = sample(c(bin,binsNo[-bin]), 1,
        p=c(probKeep, rep((1 - probKeep) / length(bins[[i]]$cuts),
        length(bins[[i]]$cuts))))
  }

  return(instance)
}
