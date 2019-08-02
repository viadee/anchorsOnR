
getFeatureWeight <- function(candidates, feature, instance, dataset){
  if (candidates$addedFeature == feature){
    added = candidates$addedPrecision
    names(added) = feature+1
    return(added)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getFeatureWeight(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset))
  }
}

getAddedCoverage <- function(candidates, feature, instance, dataset){
  if (candidates$addedFeature == feature){
    added = if(is.null(candidates$addedCoverage)) "?" else candidates$addedCoverage
    names(added) = feature+1
    return(added)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getAddedCoverage(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset))
  }
}

getFeatureText <- function(candidates, feature, instance, dataset, bins){
  bin <- bins[[feature+1]]
  if (candidates$addedFeature == feature){
    providedBin = provideBin(instance[feature+1], bin)
    if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
      featureDesc = paste(colnames(dataset)[feature+1], "=", providedBin)
    }
    else {
      featureDesc = paste(colnames(dataset)[feature+1], "IN", buildDescription(providedBin, bin$cuts, bin$right))
    }
    names(featureDesc) = feature+1
    return(featureDesc)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getFeatureText(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset, bins = bins))
  }
}

getInstanceText <- function(instance){
  lapply(colnames(instance), function(name){
    return(paste(name, "=", paste0("'",instance[name],"'")))
  })
}
