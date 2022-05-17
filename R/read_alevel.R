#' Read A_LEVEL.out
#'
#' @description A-level information is printed each time a time-dependent
#' boundary condition is specified. The information is directed to output file
#' A_LEVEL.OUT.
#' @param path full path to A_LEVEL.out file
#'
#' @return imports A_LEVEL out with tidy column names and saves metainformation
#' in attributes 'meta_general' and 'meta_units'
#' \describe{
#'   \item{time}{Time, t, at current time-level \[T\]}
#'   \item{sum_r_top}{Cumulative potential surface flux \[L\]
#'   (infiltration/evaporation: -/+)}
#'   \item{sum_r_root}{Cumulative potential transpiration \[L\]}
#'   \item{sum_v_top}{Cumulative value of the actual surface flux \[L\]
#'   (infiltration/evaporation: -/+) \[-\]}
#'   \item{sum_v_root}{Cumulative value of the actual transpiration \[L\]}
#'   \item{sum_v_bot}{Cumulative value of the bottom boundary flux \[L\]
#'   (inflow/outflow: +/-)}
#'   \item{h_top}{Pressure head at the soil surface \[L\]}
#'   \item{h_root}{Mean value of the pressure head in the soil root zone for
#'   which Beta(n)>0 \[L\]}
#'   \item{h_bot}{Pressure head at the bottom of the soil profile \[L\]}
#'   \item{a_level}{A-level number (current variable boundary condition number)
#'   \[-\]}
#' }
#' @export
#' @references \url{https://www.pc-progress.com/Downloads/Pgm_Hydrus1D/HYDRUS1D-4.17.pdf#page=277}
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_split str_trim
#' @importFrom janitor make_clean_names
#' @importFrom tibble tibble
#' @examples
#' path_alevel <- system.file("extdata/model/test/A_LEVEL.out", package = "kwb.hydrus1d")
#' alevel <- read_alevel(path = path_alevel)
#' alevel
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
                n_max = length(content) - rows_to_skip - get_number_of_endlines(content),
                readr::fwf_widths(widths = meta_units$col_width,
                                  col_names = meta_units$name)
                )

attr(alevel, "meta_general") <- meta_general
attr(alevel, "meta_units") <- meta_units

alevel

}
