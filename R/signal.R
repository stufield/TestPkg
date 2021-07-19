#' Report a Value to GUI
#'
#' Similar to `usethis::ui_*()` but does not require importing the
#' \pkg{usethis} package.
#'
#' @name signal
#' @param x,... Character. Each element of the vector becomes an entry
#' in a comma separated list and a `blue` color is added. Or `...` passed
#' directly to [cat()].
NULL

#' @describeIn signal Signal a value to the UI. Similar to
#' [usethis::ui_value()].
#' @export
value <- function(x) {
  stopifnot(is.character(x))
  x <- encodeString(x, quote = "'")
  x <- paste(x, collapse = ", ")
  if ( !isTRUE(getOption("knitr.in.progress")) ) {
    x <- paste0("\033[34m", x, "\033[39m")
  }
  structure(x, class = c("value", "character"))
}

#' @noRd
#' @export
print.value <- function(x, ..., sep = "\n") {
  cat(x, ..., sep = sep)
  invisible(x)
}

#' @describeIn signal Signal a completed task to the UI. Similar to
#' [usethis::ui_done()].
#' @export
signal_done <- function(...) {
  sym <- symbol_utf8$tick
  if ( !isTRUE(getOption("knitr.in.progress")) ) {
    sym <- paste0("\033[32m", sym, "\033[39m")   # 2714 for bold tick
  }
  cat(sym, ...)
}

#' @describeIn signal Signal a to-do task to the UI. Similar to
#' [usethis::ui_todo()].
#' @export
signal_todo <- function(...) {
  sym <- symbol_utf8$bullet
  if ( !isTRUE(getOption("knitr.in.progress")) ) {
    sym <- paste0("\033[31m", sym, "\033[39m")
  }
  cat(sym, ...)
}

#' @describeIn signal Signal oops error to the UI. Similar to
#' [usethis::ui_oops()].
#' @export
signal_oops <- function(...) {
  sym <- symbol_utf8$cross
  if ( !isTRUE(getOption("knitr.in.progress")) ) {
    sym <- paste0("\033[31m", sym, "\033[39m")
  }
  cat(sym, ...)
}

#' @describeIn signal Signal info to the UI. Similar to
#' [usethis::ui_info()].
#' @export
signal_info <- function(...) {
  sym <- symbol_utf8$info
  if ( !isTRUE(getOption("knitr.in.progress")) ) {
    sym <- paste0("\033[36m", sym, "\033[39m")
  }
  cat(sym, ...)
}

# borrowed from:
#   https://github.com/r-lib/clisymbols/blob/master/R/symbols.R
symbol_utf8 <- list(
  "tick" = "\u2714",
  "cross" = "\u2716",
  "star" = "\u2605",
  "square" = "\u2587",
  "square_small" = "\u25FB",
  "square_small_filled" = "\u25FC",
  "circle" = "\u25EF",
  "circle_filled" = "\u25C9",
  "circle_dotted" = "\u25CC",
  "circle_double" = "\u25CE",
  "circle_circle" = "\u24DE",
  "circle_cross" = "\u24E7",
  "circle_pipe" = "\u24be",
  "circle_question_mark" = "?\u20DD",
  "bullet" = "\u25CF",
  "dot" = "\u2024",
  "line" = "\u2500",
  "double_line" = "\u2550",
  "ellipsis" = "\u2026",
  "pointer" = "\u276F",
  "info" = "\u2139",
  "warning" = "\u26A0",
  "menu" = "\u2630",
  "smiley" = "\u263A",
  "mustache" = "\u0DF4",
  "heart" = "\u2665",
  "arrow_up" = "\u2191",
  "arrow_down" = "\u2193",
  "arrow_left" = "\u2190",
  "arrow_right" = "\u2192",
  "radio_on" = "\u25C9",
  "radio_off" = "\u25EF",
  "checkbox_on" = "\u2612",
  "checkbox_off" = "\u2610",
  "checkbox_circle_on" = "\u24E7",
  "checkbox_circle_off" = "\u24BE",
  "fancy_question_mark" = "\u2753",
  "neq" = "\u2260",
  "geq" = "\u2265",
  "leq" = "\u2264",
  "times" = "\u00d7",

  "upper_block_1" = "\u2594",
  "upper_block_4" = "\u2580",

  "lower_block_1" = "\u2581",
  "lower_block_2" = "\u2582",
  "lower_block_3" = "\u2583",
  "lower_block_4" = "\u2584",
  "lower_block_5" = "\u2585",
  "lower_block_6" = "\u2586",
  "lower_block_7" = "\u2587",
  "lower_block_8" = "\u2588",

  "full_block" = "\u2588"
)
