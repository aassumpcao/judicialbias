
#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
NULL

# Remove diacritics permanently
rm_accent <- function(string) {
  string %>%
    abjutils::rm_accent() %>%
    stringr::str_replace_all("[\\~\\^\\']", "")
}

# Select all text between two matches
sub_between <- function(string, pattern1, pattern2) {
  string %>%
  stringr::str_sub(
    stringr::str_locate(., pattern1)[, 2],
    stringr::str_locate(., pattern2)[, 1])
}

# Shortcut for config
vfpr_f <- httr::config(ssl_verifypeer = FALSE)

# Shortcuts for stringr functions
str_c <- stringr::str_c
replace_all <- stringr::str_replace_all
detect <- stringr::str_detect

globalVariables(c(
  "all_headers", "headers", "lead", "link", "location",
  "n_processo", "number", "rowname", "title", ".", "id"))
