listPerturbTypes = function ()
{
  c("tabular", "text", "image", "audio", "video")
}

cleanupPackageNames = function(pkgs) {

  stri_replace_all(pkgs, "", regex = "^[!_]")
}

isInIntervall <- function(intervall, wert){
  # Splitting into lower and upper boarder of the given intervall
  intervallpreprocessed <- strsplit(intervall, ",")
  lower <- intervallpreprocessed[[1]][1]
  upper <- intervallpreprocessed[[1]][2]
  # Checks for lower and upper boarder if its an open intervall
  lower_incl <- ifelse(substr(lower, 1, 1)=="(", FALSE, TRUE)
  upper_incl <- ifelse(substr(upper,nchar(upper),nchar(upper))=="(", FALSE, TRUE)
  # extracts borders as numerics
  lower <- as.numeric(gsub("\\[|\\(", "", lower))
  upper <- as.numeric(gsub("\\]|\\)", "", upper))
  # Checks if given value is in the intervall
  return((wert > lower || (wert == lower && lower_incl)) && ((wert < upper || (wert == upper && upper_incl))))
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
