makeComponent = function(component, supported = getAvailableRepresentations()) {
  checkmate::assertFunction(component)
  component = addClasses(component, c("anchors_component"))
  return(component)
}

isAnchorsComponent = function(obj) {
  return(inherits(obj, "anchors_component"))
}
