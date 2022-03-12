
# determine of RStudio is using a dark theme
# RStudio must be available; otherwise FALSE
is_dark_theme <- function() {
    identical(.Platform$GUI, "RStudio") && .rs.readUserState("theme")$isDark
}

pad <- function(x, width, side = c("right", "left", "both")) {
  side <- match.arg(side)
  just <- switch(side, right = "left", left = "right", both = "centre")
  encodeString(x, width = width, justify = just)
}

.dummy <- function() { }   # nolint

.onLoad <- function(...) {
  # this is to make the active binding switch between
  # UTF-8 and ASCII symbol encodings
  `%enc%` <- function(utf, ascii) {
    if ( getOption("cli.unicode", TRUE) && l10n_info()$`UTF-8` ) {
      utf
    } else {
      ascii
    }
  }
  pkgenv <- environment(.dummy)
  makeActiveBinding(
    "symbl", function() symbol_utf8 %enc% symbol_ascii, pkgenv
  )
  invisible()
}
