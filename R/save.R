#' Save Compressed `*.rds` File
#'
#' A thin wrapper around [saveRDS()] that ensures two things:
#' \itemize{
#'   \item The path extension is consistent and lowercase `*.rds`
#'   \item The compression used is `"xz"`, which is often optimal
#'         for our data types
#' }
#'
#' @inheritParams base::saveRDS
#' @inheritParams base::save
#' @return Returns `file`, invisibly.
#' @examples
#' \dontrun{
#' # all are the same
#' save_rds(x, "outfile.rds")
#' save_rds(x, "outfile.RDS")
#' save_rds(x, "outfile.Rds")
#'
#' # similar functionality for rda
#' # as with `save()`, must specify file argument explicitly
#' save_rda(x, file = "outfile.rda")
#'
#' # determine the compression ('xz')
#' getCompression("outfile.rda")
#' }
#' @importFrom fs path_ext
#' @importFrom usethis ui_stop ui_value
#' @export
save_rds <- function(object, file) {
  ext <- fs::path_ext(file)
  if ( "rds" != tolower(ext) ) {
    usethis::ui_stop("Incorrect file extension to `*.rds` file: {ui_value(ext)}")
  }
  fs::path_ext(file) <- tolower(ext)
  con <- xzfile(file)
  on.exit(close(con))
  saveRDS(object, con)
  invisible(file)
}

#' @describeIn save_rds
#' similar to [save_rds()], but for saving serialized `*.rda` compressed files.
#' @importFrom rlang caller_env
#' @export
save_rda <- function(..., file) {
  ext <- fs::path_ext(file)
  if ( "rda" != tolower(ext) ) {
    usethis::ui_stop("Incorrect file extension to `*.rda` file: {ui_value(ext)}")
  }
  fs::path_ext(file) <- tolower(ext)
  con <- xzfile(file)
  on.exit(close(con))
  save(..., file = con, envir = rlang::caller_env())
  invisible(file)
}

#' @describeIn save_rds
#' determine the type of compression for a serialized binary file.
#' @export
getCompression <- function(file) {
  magic <- readBin(file, what = "raw", n = 5)
  if ( all(magic[1:2] == c(31, 139)) ) {
    "gzip"
  } else if ( rawToChar(magic[1:3]) == "BZh" ) {
    "bzip2"
  } else if ( magic[1L] == 253 && rawToChar(magic[2:5]) == "7zXZ" ) {
    "xz"
  } else if ( grepl("RD[ABX][1-9]", rawToChar(magic), useBytes = TRUE) ) {
    "none"
  } else {
    "unknown"
  }
}

#' @noRd
rds_compression <- function(path) {
  files <- dir(path, full.names = TRUE, recursive = TRUE, pattern = "\\.rd[as]$")
  vapply(files, getCompression, FUN.VALUE = character(1))
}
