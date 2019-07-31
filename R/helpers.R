listPerturbTypes = function ()
{
  c("tabular", "text", "image", "audio", "video")
}

cleanupPackageNames = function(pkgs) {
  stri_replace_all(pkgs, "", regex = "^[!_]")
}

create.empty.discretization <- function(featureCount) {
  bins = list()
  for (feature in 1:featureCount) {
    bin <- list()
    bin$doDiscretize = F
    bins[[feature]] <- bin
  }
  return(bins)
}


provideBin.numeric <- function(value, cuts, right) {
  i = 1
  while (i <= length(cuts) && ((bin$right && value > cuts[i]) ||
                               (!right && value >= cuts[i]))) {
    i = i + 1
  }
  return(i)
}


plotExplanations <- function(explanations, featureNames){


  d = matrix(rep(0,length(featureNames)*length(unique(explanations[,"case"]))), ncol=length(featureNames))

  colnames(d) = featureNames

  rownames(d) = unique(explanations[,"case"])

  bins=as.data.frame(d)
  sapply(colnames(d), function(featureName){
    cases = unique(explanations[,"case"])
    sapply(cases, function(case, featureName){
      if(featureName %in% explanations[explanations[,"case"]==case, "feature"]){
        d[case, featureName]<<- explanations[explanations[,"case"]==case & explanations[,"feature"]==featureName, "feature_weight"]
        bins[case, featureName]<<- explanations[explanations[,"case"]==case & explanations[,"feature"]==featureName, "feature_desc"]
      }
    }, featureName)
  })

  par(mfrow=c(nrow(d), 1), mar=c(5, 4, 4, 7) + 0.1)
  colors=brewer.pal(n = 5, name = 'Blues')
  cuts=seq(0.2,1,0.2)
  r=sapply(1:nrow(d), function(i){
    xlab=""
    if(i==nrow(d)){
      xlab="Features"
    }
    colorBorders=sapply(1:length(d[i,]), function(x) {
      return(min(which(cuts>=d[i,x])))
    }
    )
    p<-barplot(ifelse(d[i,]==0, NA, d[i,]), axes=F, ylab=paste("Instance",i), xlab=xlab,  names.arg=colnames(d), ylim=c(0,1), col=colors[colorBorders])
    text(p, 0, ifelse(bins[i,]==0, "", substr(bins[i,], start=nchar(colnames(d))+1, stop=100000)), cex=0.7, pos=3)
  })
  legend("bottomright",legend=seq(0.2,1,0.2), fill=brewer.pal(n = 5, name = 'Blues'), xpd=TRUE, inset=c(-0.1,0))
}

buildDescription.numeric <- function(bin, cuts, right) {
  desc = ""
  if (bin == 1) {
    if (right) {
      desc = paste0("<=", cuts[1])
    } else{
      desc = paste0("<", cuts[1])
    }
  } else if (bin == length(cuts) + 1) {
    if (right) {
      desc = paste0(">", cuts[length(cuts)])
    } else{
      desc = paste0(">=", cuts[length(cuts)])
    }
  } else {
    if (right) {
      desc = paste0("(", cuts[bin - 1], ",", cuts[bin], "]")
    } else{
      desc = paste0("[", cuts[bin - 1], ",", cuts[bin], ")")
    }
  }

  return(desc)

}
