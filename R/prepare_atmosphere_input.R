#' Get Atmosphere Headers
#'
#' @return vector with atmosphere headers for "ATMOSPH.in"
#' @export
#'
#' @examples
#' get_atmosphere_headers()
get_atmosphere_headers <- function() {

  c("tAtm", "Prec", "rSoil", "rRoot", "hCritA", "rB", "hB", "ht",
    "tTop", "tBot", "Ampl", "cTop", "cBot")
}

#' Prepare Atmosphere Input
#'
#' @param inputs tibble or data.frame with user defined inputs for parameters
#'   defined in \code{get_atmosphere_headers}
#' @param defaults defaults for undefined parameters. Default:
#'   \code{\link{defaults_atmosphere}}
#' @return tibble with atmosphere values
#' @export
#' @importFrom dplyr bind_cols select
#' @importFrom tidyselect all_of
#' @importFrom kwb.utils stringList
#' @examples
#' inputs <- tibble::tibble(tAtm = 1:10, Prec = 10, rSoil = 0.4)
#' prepare_atmosphere_input(inputs)
prepare_atmosphere_input <- function(
  inputs,
  defaults = defaults_atmosphere()
)
{
  required_parameters <- get_atmosphere_headers()
  n_solutes_per_run <- 12
  optional_parameters <- sapply(2:n_solutes_per_run, function(i) {
    c(sprintf("cTop%d", i), sprintf("cBot%d", i))
  }) %>% as.vector()



  is_given <- required_parameters %in% names(inputs)
  is_given_optional <- optional_parameters %in% names(inputs)
  has_default <- required_parameters %in% names(defaults)

  if (any(is_missing <- !is_given & !has_default)) {
    stop_formatted(
      "The following required parameters were not defined: %s",
      kwb.utils::stringList(required_parameters[is_missing])
    )
  }

  dplyr::bind_cols(
    inputs[, required_parameters[is_given], drop = FALSE],
    inputs[, optional_parameters[is_given_optional], drop = FALSE],
    defaults[, required_parameters[!is_given & has_default], drop = FALSE]
  ) %>%
    dplyr::select(tidyselect::all_of(required_parameters),
                  tidyselect::all_of(optional_parameters[is_given_optional]))
}
