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
  dplyr_funs <- c(
    "filter", "slice", "arrange", "select", "rename", "distinct"
  , "mutate", "transmute", "summarise", "summarize")

  pf_ <- NULL
  var_id <- 0L
  tmp_basename <- "#tmp"

  is_magrittr_ops <- function(x) length(x) == 3 && any(as.character(x[[1]]) == ops)
  incl_magrittr_ops <- function(x) any(all.names(x) %in% ops)
  incl_dot_sym <- function(x) any(all.names(x) %in% ".")

  make_var_name <- function(base_ = tmp_basename, as_symbol = TRUE) {
    new_name <- paste0(base_, var_id)
    var_id <<- var_id + 1L
    if (as_symbol) as.symbol(new_name)
    else new_name # character
  }

  make_lambda <- function(body_) {
    body_[[1]]$rhs <- quote(._)
    arg_ <- as.vector(list(._ = quote(expr=)), "pairlist")
    call("function", arg_, wrap(body_, FALSE))
  }

  replace_dot_recursive <- function(x, expr_new) {
    if (!incl_dot_sym(x)) return(dig_ast(x))

    iter <- function(x, expr_new) {
      if (is.symbol(x) && x == ".") expr_new
      else if (length(x) <= 1 && !is.call(x)) x
      else if (x[[1]] == "~") as.call(c(quote(`~`), lapply(as.list(x[-1]), dig_ast)))
      else if (is_magrittr_ops(x)) build_fun(get_pipe_info(x), expr_new)
      else if (is.pairlist(x)) as.pairlist(lapply(x, iter, expr_new))
      else as.call(lapply(x, iter, expr_new))
    }

    iter(x, expr_new)
  }


  replace_direct_dot <- function(x, expr_new) {
    as.call(lapply(x, function(y) {
      if (is.symbol(y) && y == ".") expr_new
      else y
    }))
  }

  get_rhs_brace <- function(rhs_, sym_prev) {
    rhs_mod <- eval(rhs_, pf_)
    switch(
      typeof(rhs_mod)
      , "language" = build_fun(get_pipe_info(call("%>%", sym_prev, rhs_mod)), NULL)
      , as.call(c(rhs_mod, sym_prev))
    )
  }

  get_rhs_mod <- function(direct_dot_pos, rhs_, sym_prev) {
    if (length(rhs_) == 1 && !is.recursive(rhs_)) # symbol or character is valid when parsing
      return(as.call(c(rhs_, sym_prev)))

    rhs_elem1 <- rhs_[[1]]
    if (length(rhs_) == 1 && is.call(rhs_))
      as.call(c(rhs_elem1, sym_prev))
    else if (rhs_elem1 == "(")
      get_rhs_brace(rhs_, sym_prev)
    else if (rhs_elem1 == "{")
      replace_dot_recursive(rhs_, sym_prev)
    else if (length(direct_dot_pos) > 0 && direct_dot_pos[[1]] != 0) # 0 means converted already
      get_rhs_mod(0, replace_direct_dot(rhs_, sym_prev), sym_prev)
    else if (length(direct_dot_pos) == 0)
      get_rhs_mod(0, as.call(c(rhs_elem1, sym_prev, as.list(rhs_)[-1])), sym_prev)
    else if (direct_dot_pos == 0)# already direct-dot is converted
      replace_dot_recursive(rhs_, sym_prev)
    else
      stop("missing pattern in get_rhs_mod()")
  }

  make_last_lang <- function(acc_, first_sym, sym_prev_, reassign) {
    if (reassign) c(acc_, call("<-", first_sym, sym_prev_))
    else c(acc_, sym_prev_)
  }

  wrap <- function(lst, reassign = FALSE, use_assign_sym = FALSE) {
    sym <- make_var_name(as_symbol = !use_assign_sym)
    first_sym <- lst[[1]]$rhs
    assign_sym <- if (use_assign_sym) "assign" else "<-"

    iter2 <- function(l, sym_prev, acc = NULL) {
      if (length(l) == 0)
        return(make_last_lang(acc, first_sym, sym_prev, reassign))

      rhs_ <- l[[1]]$rhs
      op_ <- l[[1]]$op

      # need to check whether rhs_[[1]] is not "{"
      direct_dot_pos <- which(as.list(rhs_) == quote(.))
      sym_new <- make_var_name(as_symbol = !use_assign_sym)

      lang <-
        switch(as.character(op_)
          , "%T>%" = get_rhs_mod(direct_dot_pos, rhs_, sym_prev)
          , "%$%" = call(assign_sym, sym_new, call("with", sym_prev, rhs_))
          , call(assign_sym, sym_new, get_rhs_mod(direct_dot_pos, rhs_, sym_prev))
        )

      iter2(l[-1], as.symbol(`if`(op_ == "%T>%", sym_prev, sym_new)), c(acc, lang))
    }

    first_assign <- call(assign_sym, sym, first_sym)
    as.call(c(quote(`{`), iter2(lst[-1], as.symbol(sym), acc = first_assign)))
  }

  replace_rhs_origin <- function(rhs, replace_sym) {
    if (!incl_dot_sym(rhs)) return(rhs) # rhs is already applied by dig_ast() in get_pipe_info()
    else substituteDirect(rhs, list(. = replace_sym)) # maybe ok?
  }

  build_fun <- function(lst, replace_sym, use_assign_sym = FALSE) {
    origin <- lst[[1]]$rhs
    first_op <- lst[[2]]$op # `lst` should have more than one element

    if (length(origin) == 1 && origin == "." && first_op == "%>%")
      make_lambda(lst)
    else if (is.null(replace_sym))
      wrap(lst, first_op == "%<>%", use_assign_sym)
    else {
      # this is called x %>% {(. + 1) %>% f}
      lst[[1]]$rhs <- replace_rhs_origin(origin, replace_sym)
      wrap(lst, first_op == "%<>%", use_assign_sym)
    }
  }

  get_pipe_info <- function(x, acc = NULL) {
    # the most left-side of pipe-stream is needed to be recursively parsed by dig_ast()
    if (!is_magrittr_ops(x)) c(list(list(op = NULL, rhs = dig_ast(x))), acc)
    else get_pipe_info(x[[2]], c(list(list(op = x[[1]], rhs = x[[3]])), acc))
  }

  need_dplyr_modify <- function(x) {
    length(x) > 1 && any(as.character(x[[1]]) == dplyr_funs) &&
      any(vapply(as.list(x)[-1], is_magrittr_ops, logical(1)))
  }

  pre_arrange_dplyr <- function(x) {
    # x is like `filter(iris, Sepal.Width %>% is_greater_than(4.3))`
    as.call(c(x[[1]], lapply(as.list(x)[-1], function(y) {
      if (is_magrittr_ops(y)) build_fun(get_pipe_info(y), NULL, use_assign_sym = TRUE)
      else y
    })))
  }

  dig_ast <- function(x) {
    if (length(x) <= 1 && !is.recursive(x)) x
    else if (need_dplyr_modify(x)) pre_arrange_dplyr(x)
    else if (is_magrittr_ops(x)) build_fun(get_pipe_info(x), NULL)
    else if (is.pairlist(x)) as.pairlist(lapply(x, dig_ast))
    else as.call(lapply(x, dig_ast))
  }

  # returns this function
  function(expr_, eval_ = FALSE) {
    on.exit(var_id <<- 0L)

    pf_ <<- parent.frame()
    rm(list = ls(pattern = paste0("^", tmp_basename, "*"), envir = pf_, all.names = TRUE), envir = pf_)

    target <- if (is.symbol(tmp <- substitute(expr_))) expr_ else tmp
    new_call <- dig_ast(target)

    if (eval_) eval(new_call, pf_)
    else new_call
  }
})()
