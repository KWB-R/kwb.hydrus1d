#' Helper function: get number of "endlines"
#'
#' @param content content of text file
#'
#' @return returns number of lines starting with "end"
#' @export
#' @importFrom stringr str_detect
get_number_of_endlines <- function(content) {
  sum(stringr::str_detect(content, pattern = "^end"))
}
