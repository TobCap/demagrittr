is_magrittr_call <- function(x) {
  length(x) == 3 && length(x[[1]]) == 1 && any(as.character(x[[1]]) == ops)
}

is_colon_ops_call <- function(expr) {
  length(expr) == 3 && length(expr[[1]]) == 1 &&
    as.character(expr[[1]]) %in% c("::", ":::")
}

is_pipe_lambda <- function(origin, first_op) {
  length(origin) == 1 && origin == "." && first_op == "%>%"
}

is_compound_pipe <- function(expr) {
  identical(expr, quote(`%<>%`))
}

is_tee_pipe <- function(expr) {
  identical(expr, quote(`%T>%`))
}

is_dollar_pipe <- function(expr) {
  identical(expr, quote(`%$%`))
}

is_braket_call <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`{`))
}

is_paren_call <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`(`))
}

is_tilda_call <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`~`))
}

is_dot_sym <- function(expr) {
  identical(expr, quote(.))
}

has_magrittr_ops <- function(x) {
  any(all.names(x) %in% ops)
}

has_dot_sim <- function(x) {
  any(all.names(x) %in% ".")
}

has_direct_dot_arg <- function(expr) {
  is.call(expr) && any(vapply(as.list(expr)[-1], identical, FALSE, quote(.)))
}
