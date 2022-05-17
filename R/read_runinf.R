#' Read Run_Inf.out
#'
#' @description Contains time and iteration information
#' @param path full path to Run_Inf.out file
#' @return imports Run_Inf.out with tidy column names and saves metainformation
#' in attributes 'meta_general' and 'meta_units'
#' \describe{
#'   \item{tlevel}{Time-level (current time-step number) \[-\]}
#'   \item{time}{Time, t, at current time-level \[T\]}
#'   \item{dt}{Time step, delta t \[T\]}
#'   \item{itr_w}{Number of iterations necessary for solution of the water flow
#'   equation \[-\]}
#'   \item{itr_c}{Number of iterations necessary for solution of the solute
#'   transport equation \[-\]}
#'   \item{it_cum}{Cumulative number of iterations \[-\]}
#'   \item{kod_t}{Code for the boundary condition at the soil surface}
#'   \item{kod_b}{Code for the boundary condition at the bottom of the soil
#'   profile}
#'   \item{converg}{Information whether or not the numerical convergence was
#'   achieved at the current time-level}
#'   \item{peclet}{Maximum local Peclet number \[-\]}
#'   \item{courant}{Maximum local Courant number \[-\]}
#' }
#'
#' @export
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_remove_all str_replace str_split_fixed str_trim
#' @importFrom janitor make_clean_names
#' @importFrom kwb.utils multiSubstitute
#' @importFrom rlang .data
#' @references \url{https://www.pc-progress.com/Downloads/Pgm_Hydrus1D/HYDRUS1D-4.17.pdf#page=272}
#' @examples
#' path_runinf <- system.file("extdata/model/test/Run_Inf.out", package = "kwb.hydrus1d")
#' runinf <- read_runinf(path = path_runinf)
#' runinf
read_runinf <- function(path) {

  content <- readLines(path)

  meta_general <-  read_meta_general(content[3:5])
  units_list <- get_units_list(meta_general)

  col_names <- stringr::str_trim(content[8]) %>%
    stringr::str_split(pattern = "\\s+") %>%
    unlist() %>%
    janitor::make_clean_names()


  col_units <- ""

  meta_units <- tibble::tibble(name = col_names,
                               col_width = c(9,15,13,rep(5,2),9,rep(6,3), rep(10,2)),
                               unit_general = col_units,
                               unit = kwb.utils::multiSubstitute(strings = .data$unit_general,
                                                                 replacements = units_list))


  rows_to_skip <- 9
  runinf <- readr::read_fwf(file = path,
                            skip = rows_to_skip,
                            n_max = length(content)-rows_to_skip-2,
                            readr::fwf_widths(widths = meta_units$col_width,
                                              col_names = meta_units$name))

  attr(runinf, "meta_general") <- meta_general
  attr(runinf, "meta_units") <- meta_units

  runinf

}
