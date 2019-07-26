registerAnchorsComponent = function(control, component, fun, ...) {
  checkmate::assertClass(control, "anchors_control")
  checkmate::assertString(component)
  #checkmate::assertFunction(fun)
  fun.pars = list(...)
  if (!is.null(control[[component]]))
    stopf("Component '%s' already present.", component)
  control[[paste0(component, ".pars")]] = fun.pars
  control[[component]] = fun
  return(control)
}
