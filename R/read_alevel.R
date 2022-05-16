#' Read A_LEVEL.out
#'
#' @param path full path to A_LEVEL.out file
#'
#' @return imports A_LEVEL out with tidy column names
#' @export
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_split str_trim
#' @importFrom janitor make_clean_names
read_alevel <- function(path) {

col_widths <- c(12, rep(14,5),rep(11,3),8)

content <- readLines(path)


col_names <- stringr::str_trim(content[3]) %>%
  stringr::str_split(pattern = "\\s+") %>%
  unlist() %>%
  janitor::make_clean_names()

rows_to_skip <- 5

readr::read_fwf(file = path,
                skip = rows_to_skip,
                n_max = length(content)-rows_to_skip-1,
                readr::fwf_widths(widths = col_widths,
                                  col_names = col_names)
                )

}
