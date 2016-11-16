# ops <- c("%>%", "%T>%", "%$%", "%<>%")
# dplyr_funs <- c(
#   "filter", "slice", "arrange", "select", "rename", "distinct"
# , "mutate", "transmute", "summarise", "summarize")
# regexp_meta <- c(".", "\\", "|", "(", ")" , "[", "{", "^", "$", "*", "+", "?")
# varname_prefix <- "#tmp"
# devtools::use_data(ops, dplyr_funs, regexp_meta, varname_prefix,
#                    internal=TRUE, overwrite = TRUE)
pf_ <- NULL
var_id <- 0L

is_magrittr_call <- function(x) length(x) == 3 &&
  length(x[[1]]) == 1 && any(as.character(x[[1]]) == ops)
incl_magrittr_ops <- function(x) any(all.names(x) %in% ops)
incl_dot_sym <- function(x) any(all.names(x) %in% ".")

make_varname <- function(prefix = varname_prefix, as_symbol = TRUE) {
  if (any(strsplit(prefix, "")[[1]] %in% regexp_meta))
    stop("cannot use regexp_meta_char in `prefix` of make_varname()")

  new_name <- paste0(prefix, var_id)
  var_id <<- var_id + 1L

  if (exists(new_name, envir = pf_))
    Recall(prefix = prefix, as_symbol = as_symbol)
  else if (as_symbol)
    as.symbol(new_name)
  else
    new_name # character
}

rm_tmp_symbols_if_exists <- function() {
  rm(list = ls(pattern = paste0("^", varname_prefix, "*")
   , envir = pf_, all.names = TRUE)
   , envir = pf_)
}

make_lambda <- function(body_) {
  arg_ <- as.vector(list(. = quote(expr=)), "pairlist")
  call("function", arg_, wrap(body_, FALSE))
}

make_dig_with_ifs <- function(ifs_expr, env_ = parent.frame()) {
  ifs <- substitute(ifs_expr)
  if (!"expr_" %in% all.names(ifs)) stop("need to use 'expr_' in ifs clause")

  body_base <- quote(
    if (length(expr_) <= 1 && !is.recursive(expr_))
      expr_
    else if (is.pairlist(expr_))
      as.pairlist(lapply(expr_, iter_))
    else
      as.call(lapply(expr_, iter_))
  )
  add_else <- function(prev_, next_) {
    if (prev_[[1]] != "if") stop("not `if` clause")

    if (length(prev_) == 3)
      as.call(c(as.list(prev_), next_))
    else as.call(
      c(prev_[[1]], prev_[[2]], prev_[[3]], add_else(prev_[[4]], next_)))
  }

  f_body <- add_else(ifs, body_base)

  q_f <- bquote(
    function (x) {
      iter_ <- function(expr_) {
        .(f_body)
      }
      iter_(x)
    }
  )

  eval(q_f, env_)
}

replace_dot_recursive <- function(x, expr_new) {
  if (!incl_dot_sym(x)) return(dig_ast(x))

  do_func <- make_dig_with_ifs(
    if (is.symbol(expr_) && expr_ == ".")
      expr_new
    else if (length(expr_) > 1 && expr_[[1]] == "~")
      as.call(c(quote(`~`), lapply(as.list(expr_[-1]), dig_ast)))
    else if (is_magrittr_call(expr_))
      build_pipe_call(get_pipe_info(expr_), expr_new)
  )

  do_func(x)

}

replace_direct_dot <- function(x, expr_new) {
  as.call(lapply(x, function(y) {
    if (is.symbol(y) && y == ".")
      expr_new
    else
      y
  }))
}

get_rhs_brace <- function(rhs_, sym_prev) {
  rhs_mod <- eval(rhs_, pf_)
  switch(
    typeof(rhs_mod)
    , "language" = build_pipe_call(get_pipe_info(call("%>%", sym_prev, rhs_mod)), NULL)
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
  if (reassign)
    c(acc_, call("<-", first_sym, sym_prev_))
  else
    c(acc_, sym_prev_)
}

wrap <- function(lst, reassign = FALSE, use_assign_sym = FALSE) {

  sym <- make_varname(as_symbol = !use_assign_sym)
  first_sym <- lst[[1]]$rhs
  assign_sym <- if (use_assign_sym) "assign" else "<-"

  iter2 <- function(l, sym_prev, acc = NULL) {
    if (length(l) == 0)
      return(make_last_lang(acc, first_sym, sym_prev, reassign))

    rhs_ <- l[[1]]$rhs
    op_ <- l[[1]]$op

    # need to check whether rhs_[[1]] is not "{"
    direct_dot_pos <- which(as.list(rhs_) == quote(.))
    sym_new <- make_varname(as_symbol = !use_assign_sym)

    lang <-
      switch(as.character(op_)
        , "%T>%" = get_rhs_mod(direct_dot_pos, rhs_, sym_prev)
        , "%$%" = call(assign_sym, sym_new, call("with", sym_prev, dig_ast(reaplace_rhs_with_exit(rhs_,as.symbol("."),  sym_prev))))
        , call(assign_sym, sym_new, get_rhs_mod(direct_dot_pos, rhs_, sym_prev))
      )

    iter2(l[-1], as.symbol(`if`(op_ == "%T>%", sym_prev, sym_new)), c(acc, lang))
  }

  first_assign <- call(assign_sym, sym, first_sym)
  as.call(c(quote(`{`), iter2(lst[-1], as.symbol(sym), acc = first_assign)))
}

reaplace_rhs_with_exit <- function(expr, from_sym, to_sym) {
  do_func <- make_dig_with_ifs(
    if (is_magrittr_call(expr_))
      as.call(list(expr_[[1]], iter_(expr_[[2]]), expr_[[3]]))
    else if (length(expr_) == 1 && is.symbol(expr_) && identical(expr_, from_sym))
      to_sym
  )
  do_func(expr)
}

replace_rhs_origin <- function(rhs, replace_sym) {
  if (!incl_dot_sym(rhs)) return(rhs) # rhs is already applied by dig_ast() in get_pipe_info()
  else methods::substituteDirect(rhs, list(. = replace_sym)) # maybe ok?
}

build_pipe_call <- function(lst, replace_sym, use_assign_sym = FALSE) {
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
  if (!is_magrittr_call(x))
    c(list(list(op = NULL, rhs = dig_ast(x))), acc)
  else
    get_pipe_info(x[[2]], c(list(list(op = x[[1]], rhs = x[[3]])), acc))
}

need_dplyr_modify <- function(x) {
  if (length(x) > 1 && length(x[[1]]) == 1) {
    any(as.character(x[[1]]) == dplyr_funs) &&
      any(vapply(as.list(x)[-1], is_magrittr_call, logical(1)))
  } else if (length(x) > 1 && length(x[[1]]) > 1) {
    # dplyr::filter(...) or dplyr:::filter(...)
    as.character(x[[1]]) == "dplyr" &&
      any(as.character(x[[3]]) == dplyr_funs) &&
      any(vapply(as.list(x)[-1], is_magrittr_call, logical(1)))
  } else
    FALSE
}

pre_arrange_dplyr <- function(x) {
  # x is like `filter(iris, Sepal.Width %>% is_greater_than(4.3))`
  as.call(c(x[[1]], lapply(as.list(x)[-1], function(y) {
    if (is_magrittr_call(y))
      build_pipe_call(get_pipe_info(y), NULL, use_assign_sym = TRUE)
    else
      y
  })))
}

dig_ast <- make_dig_with_ifs(
  if (need_dplyr_modify(expr_))
    pre_arrange_dplyr(expr_)
  else if (is_magrittr_call(expr_))
    build_pipe_call(get_pipe_info(expr_), NULL)
)