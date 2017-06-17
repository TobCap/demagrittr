#' Decompile magrittr's syntax to normal R's syntax
#' @description Semantics of \code{\%>\%} is eager evaluation, the example of magrittr
#'  is not accurate. \code{x \%>\% f} is not equivalent to \code{f(x)} but equals to
#'  \code{{tmp <- x; f(tmp)}} or \code{{force(x); f(x)}}. demagrittr() converts magrittr's syntax to such eager
#'  evaluation.
#'
#' @param expr expression with magrittr functions such as "\%>\%"
#' @param is_NSE if TRUE, expr is not evaluated.
#'
#' @examples
#' demagrittr(x %>% f)
#' demagrittr(x %>% f(y))
#' demagrittr(x %>% f %>% g %>% h)
#'
#' demagrittr(x %>% f(y, .))
#' demagrittr(x %>% f(y, z = .))
#'
#' demagrittr(x %>% f(y = nrow(.), z = ncol(.)))
#' demagrittr(x %>% {f(y = nrow(.), z = ncol(.))})
#'
#' @export
demagrittr <- function(expr, is_NSE = TRUE) {
  env <- parent.env(environment()) # getNamespace("demagrittr")
  #assign("var_id", 0L, envir = env)
  on.exit({
    assign("var_id", 0L, envir = env)
    rm_tmp_symbols_if_exists()
  })
  assign("pf_", parent.frame(), envir = env)

  e0 <- if (is_NSE) substitute(expr) else expr

  if (typeof(e0) == "expression") {
    stop("type of expression is not supported")
  }

  new_call <- dig_ast(e0)
  new_call
}
