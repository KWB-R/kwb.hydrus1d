#' Read A_LEVEL.out
#'
#' @param path full path to A_LEVEL.out file
#'
#' @return imports A_LEVEL out with tidy column names
#' @export
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_split str_trim
#' @importFrom janitor make_clean_names
#' @importFrom tibble tibble
read_alevel <- function(path) {


content <- readLines(path)


col_names <- stringr::str_trim(content[3]) %>%
  stringr::str_split(pattern = "\\s+") %>%
  unlist() %>%
  janitor::make_clean_names()

col_units <- stringr::str_split_fixed(stringr::str_remove(content[4], "\\s+"),
                                      n = length(col_names), pattern = "\\s+") %>%
  stringr::str_remove_all("\\[|\\]")


meta_general <- NULL

meta_units <- tibble::tibble(name = col_names,
                             col_width = c(12, rep(14,5),rep(11,3),8),
                             unit_general = col_units)

rows_to_skip <- 5

alevel <- readr::read_fwf(file = path,
                skip = rows_to_skip,
                n_max = length(content)-rows_to_skip-1,
                readr::fwf_widths(widths = meta_units$col_width,
                                  col_names = meta_units$names)
                )

attr(alevel, "meta_general") <- meta_general
attr(alevel, "meta_units") <- meta_units

alevel

}
