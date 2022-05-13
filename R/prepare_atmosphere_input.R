#' Get Atmosphere Headers
#'
#' @return vector with atmosphere headers for "ATMOSPH.in"
#' @export
#'
#' @examples
#' get_atmosphere_headers()
get_atmosphere_headers <- function() {

  c("tAtm", "Prec", "rSoil", "rRoot", "hCritA", "rB", "hB", "ht",
    "tTop", "tBot", "Ampl", "cTop", "cBot", "RootDepth")
}


#' Prepare Atmosphere Input
#'
#' @param inputs tibble or data.frame with user defined inputs for parameters
#' defined in \code{get_atmosphere_headers}
#' @param defaults defaults for undefined parameters[kwb.hydrus1d::defaults_atmosphere()]
#' @return tibble with atmosphere values
#' @export
#' @importFrom dplyr bind_cols select
#' @importFrom tidyselect all_of
#' @examples
#' inputs <- tibble::tibble(tAtm = 1:10, Prec = 10, rSoil = 0.4)
#' prepare_atmosphere_input(inputs)
prepare_atmosphere_input <- function(inputs,
                                     defaults = defaults_atmosphere()) {



  paras_undefined <- names(defaults)[!names(defaults) %in% names(inputs)]
  paras_defined <- names(inputs)[names(inputs) %in% get_atmosphere_headers()]
  paras_all <- c(paras_defined, paras_undefined)

  parameters_set <- get_atmosphere_headers() %in% paras_all

  if(!all(parameters_set)) {
    unset_paras <- get_atmosphere_headers()[!parameters_set]
    unset_paras_txt <- paste0(unset_paras, collapse = ", ")
    stop(sprintf("The following required parameters were not defined: '%s'",
                 unset_paras_txt))
  }

  dplyr::bind_cols(inputs[,paras_defined],
                   defaults[, paras_undefined]) %>%
    dplyr::select(tidyselect::all_of(get_atmosphere_headers()))

}

