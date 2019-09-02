#' @title
#' Construct perturbation operator.
#'
#' @description
#' Helper function which constructs an perturbation operator.
#'
#' @note
#' In general you will not need this function, but rather one of its
#' deriviatives like \code{\link{perturbTabular}}.
#'
#' @param perturbator [\code{function}] Actual perturbation function.
#' @param supported [\code{character}] Vector of names of supported parameter representations.
#' Possible choices: \dQuote{tabular}.
#' @return [\code{anchors_perturbator}] Operator object.
#' @export

makePerturbator = function(
  perturbator,
  supported = getAvailableRepresentations())
{
  checkmate::assertFunction(perturbator)
  checkmate::assertSubset(supported, choices = getAvailableRepresentations(), empty.ok = FALSE)

  perturbator = setAttribute(perturbator, "supported", supported)
  perturbator = addClasses(perturbator, c("anchors_perturbator"))
  return(perturbator)
}

getAvailableRepresentations = function(){
  c("tabular", "image")
}
