makeRPerturbFun.tabular.nn <- function(){
  makeRPerturbFunTabular(
    cl = "tabular.nn",
    package = "stringr",
   # properties = c("factors, text, numerical"),
    name = "Nearest Neighbour Perturbation",
    short.name = "nn",
    note = ""#,
    #callees = character(0)
  )
}

perturbate.tabular.nn <- function(perturbFun, dataset, instanceIdx, anchors, ...){
  n = 5
  instance = dataset[instanceIdx,]
  subset = dataset[(instanceIdx-n):(instanceIdx+n),]
  pertCols = setdiff(seq(1, ncol(dataset), 1), anchors)

  for(i in pertCols){
    instance[,i] = sample(subset[,i],1)
  }
  return(instance)
}
