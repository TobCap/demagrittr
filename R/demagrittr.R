#' Decompile magrittr's syntax to normal R's syntax
#' @description Semantics of \code{\%>\%} is eager evaluation, the example of magrittr
#'  is not accurate. \code{x \%>\% f} is not equivalent to \code{f(x)} but equals to
#'  \code{{tmp <- x; f(tmp)}} or \code{{force(x); f(x)}}. demagrittr() converts magrittr's syntax to such eager
#'  evaluation.
#'
#' @param expr expression with magrittr functions such as "\%>\%"
#' @param is_NSE if TRUE, expr is not evaluated.
#' @param as_lazy if TRUE, pipe streams are formed into nest of call offunctions.
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
demagrittr <- function(expr, is_NSE = TRUE, mode = c("eager", "lazy", "promise")) {
  ## Initialize variables that are used for side-effect purpose
  init_(pf_ = parent.frame(), mode = match.arg(mode))
  on.exit({init_(pf_ = emptyenv(), mode = NULL)})

  e0 <- if (is_NSE) substitute(expr) else expr

  if (typeof(e0) == "expression") {
    stop("type of expression is not supported")
  }

  new_call <- dig_ast(e0)
  new_call
}
