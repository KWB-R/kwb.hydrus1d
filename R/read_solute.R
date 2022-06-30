#' Read solute1.out
#'
#' @description Reads Solute output
#'
#' @param path full path to solute_id.out file (default: system.file("extdata/model/test/solute1.out",
#' package = "kwb.hydrus1d"))
#' @param dbg show debug messages (default: FALSE)
#' @return imports solute_id.out with tidy column names and saves metainformation
#' in attributes 'meta_general' and 'meta_units'
#' \describe{
#'   \item{time}{Time, t, at current time-level \[T\]}
#'   \item{cv_top}{Actual solute flux across the soil surface \[ML-2 T -1\] (inflow/outflow: +/-)}
#'   \item{cv_bot}{Actual solute flux across the bottom of the soil profile \[ML-2 T -1\]
#'   (inflow/outflow: +/-)}
#'   \item{sum_cv_top}{Cumulative solute flux across the soil surface \[ML-2\]
#'   (inflow/outflow: +/-)}
#'   \item{sum_cv_bot}{Cumulative solute flux across the bottom of the soil
#'   profile \[ML-2\] (inflow/outflow: +/-)}
#'   \item{sum_cv_ch0}{Cumulative amount of solute added to the flow region by
#'   zero-order reactions \[ML-2\] (negative when removed from the system)}
#'   \item{sum_cv_ch1}{Cumulative amount of solute removed from the flow region
#'   by first-order reactions \[ML-2\] (negative removed from the system).}
#'   \item{c_top}{Solute concentration at the soil surface \[ML-3\]}
#'   \item{c_root}{Mean solute concentration of the root zone \[ML-3\]}
#'   \item{c_bot}{Solute concentration at the bottom of the soil profile \[ML-3\]}
#'   \item{cv_root}{Actual root solute uptake in the root zone \[ML-2 T -1\]
#'   (positive when removed from the system)}
#'   \item{sum_cv_root}{Cumulative amount of solute removed from the flow region
#'   by root water uptake S \[ML-2\] (positive when removed from the system)}
#'   \item{sum_cv_n_eql}{Cumulative mass transfer to either kinetic adsorption
#'   sites (type-2 adsorption sites), or to the immobile liquid region \[ML-2\]
#'   (inflow/outflow: +/-).}
#'   \item{t_level}{Time-level (current time-step number) \[-\]}
#'   \item{c_gwl}{Average concentration in the saturated zone \[ML-3\] (in the
#'   groundwater)}
#'   \item{c_run_off}{Solute flux in the runoff (\[ML-3\] * \[LT-1\]) \[ML-2T-1\]}
#'   \item{sum_c_run_off}{Cumulative solute flux in the runoff (\[ML-3\] * \[LT-1\]* \[T\])
#'   \[ML-2\]}
#'   \item{cv_i}{Solute flux at the first through third observation node
#'   (\[ML-3\] * \[LT-1\]) \[ML-2T-1\]}
#'   \item{sum_cv_i}{cumulative solute flux at the first through third observation
#'   node (\[ML-3\] * \[LT-1\] * \[T\]) \[ML-2\]. Total solute fluxes are reported
#'   when only one solute is simulated. Only convective fluxes (for the first
#'   solute) are reported when 2 or more solutes are simulated}
#' }
#' @export
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_remove_all str_replace str_split_fixed str_trim
#' @importFrom janitor make_clean_names
#' @importFrom kwb.utils multiSubstitute safePath
#' @importFrom rlang .data
#' @importFrom utils read.fwf
#' @references \url{https://www.pc-progress.com/Downloads/Pgm_Hydrus1D/HYDRUS1D-4.17.pdf#page=273}
#' @examples
#' path_solute <- system.file("extdata/model/test/solute1.out", package = "kwb.hydrus1d")
#' solute <- read_solute(path = path_solute)
#' solute
read_solute <- function(path = system.file("extdata/model/test/solute1.out", package = "kwb.hydrus1d"),
                        dbg = FALSE)
{

  path <- kwb.utils::safePath(path)
  path_balance <- kwb.utils::safePath(dirname(path), "Balance.out")

  meta_general <- read_meta_general(readLines(path_balance)[3:5])

  content <- readLines(path)

  col_names <- stringr::str_trim(content[3L]) %>%
    stringr::str_remove(", i=1,NObs\\)") %>%
    stringr::str_split(pattern = "\\s+") %>%
    unlist() %>%
    janitor::make_clean_names()

  col_widths <- c(14L, rep(13L, 12L), 8L, rep(13L, 5L))

  as.character(t(utils::read.fwf(textConnection(content[4L]),
                  widths = col_widths))[,1])

  col_units <- as.character(t(utils::read.fwf(textConnection(content[4L]),
                                              widths = col_widths))[,1]) %>%
    stringr::str_remove("\\s+") %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\\[|\\]") %>%
    stringr::str_replace_na("")

  meta_units <- tibble::tibble(
    name = col_names,
    col_width = col_widths,
    unit_general = col_units,
    unit = kwb.utils::multiSubstitute(
      strings = col_units,
      replacements = get_units_list(meta_general)
    )
  )

  rows_to_skip <- 4L

  solute <- readr::read_fwf(
    file = path,
    skip = rows_to_skip,
    n_max = length(content) - rows_to_skip - get_number_of_endlines(content),
    readr::fwf_widths(
      widths = meta_units$col_width,
      col_names = meta_units$name
    ),
    show_col_types = dbg
    )

  set_metadata(solute, meta_general, meta_units)
}
