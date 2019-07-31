listPerturbTypes = function ()
{
  c("tabular", "text", "image", "audio", "video")
}

cleanupPackageNames = function(pkgs) {

  stri_replace_all(pkgs, "", regex = "^[!_]")
}


provideBin.numeric <- function(value, cuts, right){
  if(right){
    i=1
    while(i <=length(cuts) & value > cuts[i]){
      i=i+1
    }
    return(i)
  }else{
    i=1
    while(i <=length(cuts) & value >= cuts[i]){
      i=i+1
    }
    return(i)
  }
}


buildDescription.numeric <- function(bin, cuts, right){

  desc=""
  if(bin==1){
    if(right){
      desc=paste0("<=",cuts[1])
    }else{
      desc=paste0("<",cuts[1])
    }
  }else if(bin==length(cuts)+1){

    if(right){
      desc=paste0(">", cuts[length(cuts)])
    }else{
      desc=paste0(">=", cuts[length(cuts)])
    }

  }else{
    if(right){
      desc=paste0("(", cuts[bin-1],",", cuts[bin], "]")
    }else{
      desc=paste0("[", cuts[bin-1], ",",cuts[bin], ")")
    }

  }

  return(desc)

}
