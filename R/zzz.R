.onAttach <- function(...) {
  unlockBinding("pf_", getNamespace("demagrittr"))
  unlockBinding("var_id", getNamespace("demagrittr"))
}
