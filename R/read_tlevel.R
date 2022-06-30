#' Read T_LEVEL.out
#'
#' @description Stores pressure heads and fluxes on the boundaries and in the
#' root zone.
#'
#' @param path full path to T_LEVEL.out file
#' @param dbg show debug messages (default: FALSE)
#' @return imports T_LEVEL out with tidy column names and  saves metainformation
#' in attributes 'meta_general' and 'meta_units'
#' \describe{
#'   \item{time}{Time, t, at current time-level \[T\]}
#'   \item{r_top}{Potential surface flux \[LT-1\] (infiltration/evaporation: -/+)}
#'   \item{r_root}{Potential transpiration rate \[LT-1\]}
#'   \item{v_top}{Actual surface flux \[LT-1\] (infiltration/evaporation: -/+)}
#'   \item{v_root}{Actual transpiration rate \[LT-1\]}
#'   \item{v_bot}{Actual flux across the bottom of the soil profile \[LT-1\]
#'   (inflow/outflow +/-)}
#'   \item{sum_r_top}{Cumulative value of the potential surface flux \[L\]
#'   (infiltration/evaporation: -/+)}
#'   \item{sum_r_root}{Cumulative value of the potential transpiration rate
#'   \[L\] }
#'   \item{sum_v_top}{Cumulative value of the actual surface flux \[L\]}
#'   \item{sum_v_root}{Cumulative value of the actual transpiration rate \[L\]
#'   (infiltration/evaporation: -/+)}
#'   \item{sum_v_bot}{Cumulative value of the actual flux across the bottom of
#'    the soil profile \[LT-1\] (inflow/outflow +/-)}
#'   \item{h_top}{Pressure head at the soil surface \[L\]}
#'   \item{h_root}{Mean value of the pressure head over the region for which
#'   Beta(n) > 0 (i.e. within the root zone) \[L\]}
#'   \item{h_bot}{Pressure head at the bottom of the soil profile \[L\]}
#'   \item{run_off}{Surface runoff \[LT-1\]}
#'   \item{sum_run_off}{Cumulative surface runoff \[L\]}
#'   \item{volume}{Volume of water in the entire flow domain \[L\]}
#'   \item{sum_infil}{Cumulative infiltration \[L\]}
#'   \item{sum_evap}{Cumulative actual evaporation \[L\]}
#'   \item{t_level}{Time-level (current time-step number) \[-\]}
#'   \item{cum_w_trans}{Cumulative mass transfer between the mobile and immobile
#'   regions for dual porosity model \[L\]}
#'   \item{snow_layer}{Thickness of snow layer, expressed as the "snow water
#'   equivalent" (the amount of water contained within the snowpack) \[L\]}
#' }
#' @export
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_remove_all str_replace str_split_fixed str_trim
#' @importFrom janitor make_clean_names
#' @importFrom kwb.utils multiSubstitute
#' @importFrom rlang .data
#' @references \url{https://www.pc-progress.com/Downloads/Pgm_Hydrus1D/HYDRUS1D-4.17.pdf#page=271}
#' @examples
#' path_tlevel <- system.file("extdata/model/test/T_LEVEL.out", package = "kwb.hydrus1d")
#' tlevel <- read_tlevel(path = path_tlevel)
#' tlevel
read_tlevel <- function(path, dbg = FALSE)
{
  content <- readLines(path)

  meta_general <-  read_meta_general(content[3:5])

  col_names <- stringr::str_trim(content[7L]) %>%
    stringr::str_split(pattern = "\\s+") %>%
    unlist() %>%
    janitor::make_clean_names()

  col_units <- stringr::str_split_fixed(
    stringr::str_remove(content[8L], "\\s+"),
    n = length(col_names), pattern = "\\s+"
  ) %>%
    stringr::str_remove_all("\\[|\\]")

  meta_units <- tibble::tibble(
    name = col_names,
    col_width = c(rep(13L, 19L), 7L, 13L, 11L),
    unit_general = col_units,
    unit = kwb.utils::multiSubstitute(
      strings = .data$unit_general,
      replacements = get_units_list(meta_general)
    )
  )

  rows_to_skip <- 9L

  tlevel <- readr::read_fwf(
    file = path,
    skip = rows_to_skip,
    n_max = length(content) - rows_to_skip - get_number_of_endlines(content),
    readr::fwf_widths(
      widths = meta_units$col_width,
      col_names = meta_units$name
    ),
    show_col_types = dbg
  )

  set_metadata(tlevel, read_meta_general(content[3:5]), meta_units)
}
