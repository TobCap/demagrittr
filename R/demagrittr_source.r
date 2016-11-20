#' Convert R's source file to non \%>\% code.
#' @description read file and convert the code which uses %>%
#'   to flatten code
#'
#' @param in_ file path of input.
#' @param out_ file path of output.
#' @param ask ask to overwrite when `out_`` already exists
#'
#' @return no return. side effect on a file.
#'
#' @examples
#' tmp_dir <- tempdir()
#' in_path <- file.path(tmp_dir, "test_in.r")
#' out_path <- file.path(tmp_dir, "test_out.r")
#'
#' writeLines(
#' "
#' x <- data.frame(a = 1:5, b = 6:10)
#' y <- x %>%
#'   select(b) %>%
#'   filter(b >= 8)
#' ", in_path)
#'
#' demagrittr_source(in_path, out_path, ask=FALSE)
#'
#' # input file
#' cat(paste0(readLines(in_path), collapse="\n"))
#' # output file
#' cat(paste0(readLines(out_path), collapse="\n"))
#'
#'
#' @export
demagrittr_source <- function(in_, out_, ask = TRUE) {
  stopifnot(file.exists(in_))

  if (ask && file.exists(out_)) {
    ans <- readline(prompt = paste0(out_, " exists. do you allow over write? (y/n) \n"))
    if (!tolower(ans) %in% c("y", "yes")) {
        stop("stopped")
    }
  } else {
    out_fullpath <- normalizePath("b.txt", mustWork = FALSE)
    out_dir <- dirname(out_fullpath)
    if (!dir.exists(out_dir))
      stop("output directory does not exist.")
  }

  e1 <- parse(file = in_, keep.source = FALSE)
  # e2 <- as.call(c(quote(`{`), as.list(e1)))
  # e3 <- demagrittr(e2, FALSE)
  # on.exit(closeAllConnections())
  # writeLines(deparse(e3), out_)

  e2 <- lapply(e1, demagrittr, is_NSE=FALSE)
  e3 <- paste(unlist(sapply(e2, deparse, width.cutoff = 500L)), collapse = "\n")
  con_ <- file(out_, "w")
  on.exit(close(con_))
  writeLines(e3, con_)
}
