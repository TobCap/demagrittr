# ops <- c("%>%", "%T>%", "%$%", "%<>%")
# regexp_meta <- c(".", "\\", "|", "(", ")" , "[", "{", "^", "$", "*", "+", "?")
# varname_prefix <- "#tmp"
# devtools::use_data(ops, regexp_meta, varname_prefix,
#                    internal=TRUE, overwrite = TRUE)
pf_ <- NULL
var_id <- 0L
as_lazy <- FALSE
utils::globalVariables(c("expr_", "iter_"))

is_magrittr_call <- function(x) {
  length(x) == 3 && length(x[[1]]) == 1 && any(as.character(x[[1]]) == ops)
}

has_magrittr_ops <- function(x) {
  any(all.names(x) %in% ops)
}

has_dot_sim <- function(x) {
  any(all.names(x) %in% ".")
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

has_direct_dot <- function(expr) {
  is.call(expr) && any(vapply(expr, identical, FALSE, quote(.)))
}

init_ <- function(pf_, as_lazy) {
  pkg_env <- parent.env(environment()) # getNamespace("demagrittr")

  rm_tmp_symbols_if_exists(pf_)
  assign("var_id", 0L, envir = pkg_env)
  assign("as_lazy", as_lazy, envir = pkg_env)
  assign("pf_", pf_, envir = pkg_env)

  invisible()
}


make_varname <- function(prefix = varname_prefix) {
  if (any(strsplit(prefix, "")[[1]] %in% regexp_meta)) {
    stop("cannot use regexp_meta_char in `prefix` of make_varname()")
  }

  new_name <- paste0(prefix, var_id)
  var_id <<- var_id + 1L

  if (exists(new_name, envir = pf_)) {
    Recall(prefix = prefix)
  } else {
    as.symbol(new_name)
  }
}

rm_tmp_symbols_if_exists <- function(env) {
  rm(list = ls(pattern = paste0("^", varname_prefix, "*")
   , envir = env, all.names = TRUE)
   , envir = env)
}

make_lambda <- function(body_) {
  arg_ <- as.vector(list(. = quote(expr=)), "pairlist")
  call("function", arg_, wrap(body_))
}

make_lambda_lazy <- function(body_) {
  # change format from `.` to `..` to prevent recursive transform of `.`
  arg_ <- as.vector(list(.. = quote(expr=)), "pairlist")
  body_[[1]]$rhs <- quote(..)
  call("function", arg_, wrap_lazy(body_))
}

construct_lang_manipulation <- function(ifs_expr, env_ = parent.frame()) {
  ifs <- substitute(ifs_expr)
  if (!"expr_" %in% all.names(ifs)) {
    stop("need to use 'expr_' in ifs clause")
  }

  body_base <- quote(
    if (length(expr_) <= 1 && !is.recursive(expr_)) {
      expr_
    } else if (is.pairlist(expr_)) {
      as.pairlist(lapply(expr_, iter_))
    } else {
      as.call(lapply(expr_, iter_))
    }
  )

  add_else <- function(prev_, next_) {
    if (prev_[[1]] != "if") {
      stop("not `if` clause")
    }

    if (length(prev_) == 3) {
      as.call(c(as.list(prev_), next_))
    } else {
      as.call(c(prev_[[1]], prev_[[2]], prev_[[3]], add_else(prev_[[4]], next_)))
    }
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
  if (!has_dot_sim(x)) {
    # for short-cut porpose
    return(dig_ast(x))
  }

  do_func <- construct_lang_manipulation(
    if (is.symbol(expr_) && expr_ == ".") {
      expr_new
    } else if (length(expr_) > 1 && expr_[[1]] == "~") {
      as.call(c(quote(`~`), lapply(as.list(expr_[-1]), dig_ast)))
    } else if (is_magrittr_call(expr_)) {
      build_pipe_call(expr_, expr_new)
    }
  )

  do_func(x)

}

replace_direct_dot <- function(x, expr_new) {
  as.call(lapply(x, function(y) {
    if (is.symbol(y) && y == ".") {
      expr_new
    } else {
      y
    }
  }))
}

get_rhs_paren <- function(rhs_, sym_prev) {
  # magrittr can evaluate below language syntax
  # language: `1:10 %>% (substitute(f(), list(f = sum)))`
  # As vignette says in https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
  # `Whenever you want to use a function- or call-generating statement as
  # right-hand side, parentheses are used to evaluate the right-hand side
  # before piping takes place.`.

  # closure:
  # `1 %>% (function(x) x + 1))' runs
  # '1 %>% (2 %>% (function(x) function(y) x + y))` occurs error
  # '1 %>% (2 %>% (function(x) {force(x); function(y) x + y}))` runs

  rhs_mod <- eval(rhs_, pf_)
  # browser()
  switch(
    typeof(rhs_mod)
    , "language" = {
        if (class(rhs_mod[[1]]) == "function") {
          # N.B. These are different. The first case is handled in this clause.
          # 1:10 %>% (substitute(f(), list(f = sum)) -> as.call(list(sum, 1))
          # 1:10 %>% (substitute(f(), list(f = quote(sum))) -> as.call(list(quote(sum), 1))
          if (is.primitive(rhs_mod[[1]])) {
            rhs_mod[[1]] <- as.symbol(asNamespace("methods")$.primname(rhs_mod[[1]]))
          } else {
            # FIX-ME: is there another way?
            rhs_mod[[1]] <- parse(text = deparse(rhs_mod[[1]], width.cutoff = 500L))[[1]]
          }
        }
        build_pipe_call(call("%>%", sym_prev, rhs_mod), NULL)
      }
    , as.call(c(dig_ast(rhs_), sym_prev))
  )
}



wrap_lazy <- function(lst) {

  iter <- function(l, acc) {
    if (length(l) == 0) {
      return(acc)
    }

    rhs_ <- l[[1]]$rhs
    op_ <- l[[1]]$op

    body_ <-
      if (is_dollar_pipe(op_)) {
        call("with", acc, replace_dot_recursive(rhs_, acc))
      } else if (is.symbol(rhs_)) {
        as.call(list(rhs_, acc))
      } else if (is_paren_call(rhs_)) {
        get_rhs_paren(rhs_, acc)
      } else if (is_braket_call(rhs_)) {
        replace_dot_recursive(rhs_, acc)
      } else if (has_direct_dot(rhs_)) {
        rhs_mod <- replace_direct_dot(rhs_, acc)
        replace_dot_recursive(rhs_mod, acc)
      } else if (!has_direct_dot(rhs_)) {
        rhs_mod <- add_first_dot_to_rhs(rhs_, acc)
        replace_dot_recursive(rhs_mod, acc)
      } else {
        stop("missing pattern in get_rhs_mod_lazy()")
      }

    if (is_tee_pipe(op_)) {
      call("{", build_pipe_call(call("%>%", acc, rhs_), NULL), iter(l[-1], acc))
      # T_body <-  bquote({
      #     .(prev_call)
      #     .(next_call)
      #   }, list(prev_call = build_pipe_call(call("%>%", quote(..), rhs_), NULL),
      #           next_call = iter(l[-1], quote(..))
      #           )
      #   )
      #
      # as.call(c(call("(", call("function", as.pairlist(alist(..=)), T_body)), acc))

    } else {
      iter(l[-1], body_)
    }

  }
  iter(lst[-1], lst[[1]]$rhs)

}

wrap <- function(lst) {

  iter2 <- function(l, sym_prev, acc = NULL) {
    if (length(l) == 0) {
      return(c(acc, sym_prev))
    }

    rhs_ <- l[[1]]$rhs
    op_ <- l[[1]]$op

    body_ <-
      if (is_dollar_pipe(op_)) {
        call("with", sym_prev, replace_dot_recursive(rhs_, sym_prev))
      } else if (is.symbol(rhs_)) {
        as.call(c(rhs_, sym_prev))
      } else if (is_paren_call(rhs_)) {
        get_rhs_paren(rhs_, sym_prev)
      } else if (is_braket_call(rhs_)) {
        replace_dot_recursive(rhs_, sym_prev)
      } else if (has_direct_dot(rhs_)) {
        rhs_mod <- replace_direct_dot(rhs_, sym_prev)
        replace_dot_recursive(rhs_, sym_prev)
      } else if (!has_direct_dot(rhs_)) {
        rhs_mod <- add_first_dot_to_rhs(rhs_, sym_prev)
        replace_dot_recursive(rhs_mod, sym_prev)
      } else {
        stop("missing pattern in iter2()")
      }

    if (is_tee_pipe(op_)) {
      iter2(l[-1], sym_prev, c(acc, body_))
    } else {
      sym_new <- make_varname()
      iter2(l[-1], sym_new, c(acc, call("<-", sym_new, body_)))
    }
  }

  first_sym <- make_varname()
  first_assign <- call("<-", first_sym, lst[[1]]$rhs)
  as.call(c(quote(`{`), iter2(lst[-1], first_sym, acc = first_assign)))
}

replace_rhs_origin <- function(rhs, replace_sym) {
  if (!has_dot_sim(rhs)) {
    # rhs is already applied by dig_ast()
    return(rhs)
  } else {
    # maybe ok?
    methods::substituteDirect(rhs, list(. = replace_sym))
  }
}

add_first_dot_to_rhs <- function(rhs, new_call) {
  ## rhs[[1]] should be passed recuresively
  # > demagrittr(1 %>% (. %>% exp)(), as_lazy = TRUE)
  # (function(.) exp(.))(1)
  as.call(c(dig_ast(rhs[[1]]), new_call, as.list(rhs)[-1]))
}

build_pipe_call <- function(expr, replace_sym, use_assign_sym = FALSE) {
  # `lst` should have more than one element
  lst <- get_pipe_info(expr)
  origin <- lst[[1]]$rhs
  first_op <- lst[[2]]$op

  body_ <-
    if (as_lazy) {
      if (is_pipe_lambda(origin, first_op)) {
        make_lambda_lazy(lst)
      } else if (is.null(replace_sym)) {
        wrap_lazy(lst)
      } else {
        lst[[1]]$rhs <- replace_rhs_origin(origin, replace_sym)
        wrap_lazy(lst)
      }
    } else {
      # as eager
      if (is_pipe_lambda(origin, first_op)) {
        make_lambda(lst)
      } else if (is.null(replace_sym)) {
        wrap(lst)
      } else {
        # this is called x %>% {(. + 1) %>% f}
        lst[[1]]$rhs <- replace_rhs_origin(origin, replace_sym)
        wrap(lst)
      }
    }

  if (is_compound_pipe(first_op)) {
    call("<-", origin, body_)
  } else {
    body_
  }
}

get_pipe_info <- function(x, acc = NULL) {
  # the most left-side of pipe-stream is needed to be recursively
  # parsed by dig_ast()
  if (!is_magrittr_call(x)) {
    c(list(list(op = NULL, rhs = dig_ast(x))), acc)
  } else {
    get_pipe_info(x[[2]], c(list(list(op = x[[1]], rhs = x[[3]])), acc))
  }
}


dig_ast <- construct_lang_manipulation(
  if (is_magrittr_call(expr_)) {
    build_pipe_call(expr_, NULL)
  }
)
