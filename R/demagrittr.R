#' Decompile magrittr's syntax to normal R's syntax
#' @description Semantics of \code{\%>\%} is eager evaluation, the example of magrittr
#'  is not accurate. \code{x \%>\% f} is not equivalent to \code{f(x)} but equals to
#'  \code{{tmp <- x; f(tmp)}} or \code{{force(x); f(x)}}. demagrittr() converts magrittr's syntax to such eager
#'  evaluation.
#'
#' @param expr_ expression with magrittr functions such as "\%>\%"
#' @param eval_ evaluate the result if value is TRUE
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
demagrittr <- (function() {

  ops <- c("%>%", "%T>%", "%$%", "%<>%")
  pf_ <- NULL
  var_id <- 0L

  make_var_name <- function(base_ = "._tmp") {
    new_name <- paste0(base_, var_id)
    var_id <<- var_id + 1L
    as.symbol(new_name)
  }

  make_lambda <- function(body_) {
    body_[[1]]$rhs <- quote(._)
    call("function", as.pairlist(alist(._=)), wrap(body_, FALSE))
  }

  replace_lhs <- function(x, expr_new) {
    as.call(c(x[[1]], replace_dot_recursive(x[[2]], expr_new), x[[3]]))
  }

  replace_dot_recursive <- function(x, expr_new) {
    # inside tilda, kind of inner DSL, a dot symbol is reserved for model description
    if ((length(x) <= 1 && x != ".") || (is.call(x) && x[[1]] == "~")) x
    else if (is.symbol(x) && x == ".") expr_new
    else if (length(x) == 3 && as.character(x[[1]]) %in% ops) get_pipe_info(replace_lhs(x, expr_new), build_fun)
    else if (is.pairlist(x)) as.pairlist(lapply(x, replace_dot_recursive, expr_new))
    else as.call(lapply(x, replace_dot_recursive, expr_new))
  }

  replace_direct_dot <- function(x, expr_new) {
    as.call(lapply(x, function(y) {
      y_syms <- all.names(y)
      if (length(y) == 1 && is.symbol(y) && y == ".") expr_new
      else if (any("." %in% y_syms) && !any(y_syms %in% ops)) replace_dot_recursive(y, expr_new)
      else if (any(y_syms %in% ops)) dig_ast(y)
      else y
    }))
  }

  get_rhs_brace <- function(rhs_, sym_prev) {
    rhs_mod <- eval(rhs_, pf_)

    switch(
      typeof(rhs_mod)
      , "language" = get_pipe_info(call("%>%", sym_prev, rhs_mod), build_fun)
      , as.call(c(rhs_mod, sym_prev))
    )
  }

  get_rhs_mod <- function(direct_dot_pos, rhs_, sym_prev) {
    if (length(direct_dot_pos) > 0)
      replace_direct_dot(rhs_, sym_prev)
    else if (length(rhs_) == 1 && is.recursive(rhs_))
      as.call(c(rhs_[[1]], sym_prev))
    else if (length(rhs_) == 1)
      as.call(c(rhs_, sym_prev))
    else if (rhs_[[1]] == "(")
      get_rhs_brace(rhs_, sym_prev)
    else if (rhs_[[1]] == "{")
      replace_dot_recursive(rhs_, sym_prev)
    else
      as.call(c(rhs_[[1]], sym_prev, lapply(as.list(rhs_)[-1], replace_dot_recursive, sym_prev)))
  }

  wrap <- function(lst, reassign = FALSE) {
    sym <- make_var_name()
    first_sym <- lst[[1]]$rhs

    make_last_lang <- function(acc_, sym_prev_) {
      if (reassign) c(acc_, call("<-", first_sym, sym_prev_))
      else c(acc_, sym_prev_)
    }

    iter2 <- function(l, sym_prev, acc = NULL) {
      if (length(l) == 0) return(make_last_lang(acc, sym_prev))

      rhs_ <- l[[1]]$rhs
      op_ <- l[[1]]$op
      direct_dot_pos <- which(as.list(rhs_) == quote(.))

      lang <-
        switch(as.character(l[[1]]$op)
          , "%T>%" = get_rhs_mod(direct_dot_pos, rhs_, sym)
          , "%$%" = call("<-", sym, call("with", sym, rhs_))
          , call("<-", sym, get_rhs_mod(direct_dot_pos, rhs_, sym))
          # last part is a default value for "%>%" and "%<>%"
        )

      # first assignment
      lang <- c(`if`(is.null(acc), call("<-", sym, sym_prev), NULL), lang)
      iter2(l[-1], sym, c(acc, lang))
    }
    as.call(c(quote(`{`), iter2(lst[-1], first_sym)))
  }

  build_fun <- function(lst) {
    origin <- lst[[1]]$rhs
    first_op <- lst[[2]]$op # `lst` should have more than one element

    if (length(origin) == 1 && origin == "." && first_op == "%>%")
      make_lambda(lst)
    else
      wrap(lst, first_op == "%<>%")
  }

  # CPS form
  get_pipe_info <- function(x, cont) {
    if (length(x) <= 1 || !(as.character(x[[1]]) %in% ops)) cont(list(list(op = NULL, rhs = dig_ast(x))))
    else get_pipe_info(x[[2]], function(y) cont(c(y, list(list(op = x[[1]], rhs = x[[3]])))))
  }

  dig_ast <- function(x) {
    if (length(x) <= 1 && !is.recursive(x)) x
    else if (length(x) == 3 && as.character(x[[1]]) %in% ops) get_pipe_info(x, build_fun)
    else if (is.pairlist(x)) as.pairlist(lapply(x, dig_ast))
    else as.call(lapply(x, dig_ast))
  }

  # returns this function
  function(expr_, eval_ = FALSE) {
    on.exit(var_id <<- 0L)

    pf_ <<- parent.frame()
    target <- if (is.symbol(tmp <- substitute(expr_))) expr_ else tmp
    new_call <- dig_ast(target)

    if (eval_) eval(new_call, pf_)
    else new_call
  }
})()
