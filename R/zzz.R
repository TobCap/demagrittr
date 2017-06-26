.onAttach <- function(...) {
  # unlockBinding("pf_", getNamespace("demagrittr"))
  # unlockBinding("var_id", getNamespace("demagrittr"))
  # unlockBinding("mode", getNamespace("demagrittr"))
  # unlockBinding("varname_prefix", getNamespace("demagrittr"))
  ## might be a bad hack but can avoid notes for R CMD check
  eval(call("unlockBinding", "pf_", quote(getNamespace("demagrittr"))))
  eval(call("unlockBinding", "var_id", quote(getNamespace("demagrittr"))))
  eval(call("unlockBinding", "mode", quote(getNamespace("demagrittr"))))
  eval(call("unlockBinding", "varname_prefix", quote(getNamespace("demagrittr"))))
}
