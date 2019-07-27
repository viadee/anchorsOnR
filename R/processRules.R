
getFeatureWeight <- function(candidates, feature, instance, dataset, datasetDisc){
  if (candidates$addedFeature == feature){
    added = candidates$addedPrecision
    names(added) = feature+1
    return(added)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getFeatureWeight(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset, datasetDisc = datasetDisc))
  }
}

getAddedCoverage <- function(candidates, feature, instance, dataset, datasetDisc){
  if (candidates$addedFeature == feature){
    added = if(is.null(candidates$addedCoverage)) "?" else candidates$addedCoverage
    names(added) = feature+1
    return(added)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getAddedCoverage(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset, datasetDisc = datasetDisc))
  }
}

getFeatureText <- function(candidates, feature, instance, dataset, datasetDisc){
  if (candidates$addedFeature == feature){
    featureDesc = paste(colnames(dataset)[feature+1], "IN INLC RANGE", datasetDisc[as.numeric(rownames(instance)),feature+1])
    names(featureDesc) = feature+1
    return(featureDesc)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getFeatureText(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset, datasetDisc = datasetDisc))
  }
}

getInstanceText <- function(instance){
  lapply(colnames(instance), function(name){
    return(paste(name, "=", paste0("'",instance[name],"'")))
  })
}
