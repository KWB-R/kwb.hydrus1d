#' Convert Atmosphere to String
#'
#' @param atm  atm as retrieved by \code{prepare_atmosphere_data}
#'
#' @return tibble with strings padded properly for Hydrus1D
#' @export
#' @importFrom stringr str_pad
#'
convert_atmosphere_to_string <- function(atm) {

  pad_short <- 11
  pad_long <- 12

  headers_short <- "tAtm"
  headers_long <- get_atmosphere_headers()[!get_atmosphere_headers() %in% headers_short]

  atm[headers_short] <- (lapply(atm[headers_long],
                                function(x) stringr::str_pad(x,
                                                             pad_short,
                                                             "left")))
  atm[headers_long] <- (lapply(atm[headers_long],
                               function(x) stringr::str_pad(x,
                                                            pad_long,
                                                            "left")))

  bool_short <- names(atm) %in% headers_short
  names(atm)[bool_short] <-  stringr::str_pad(names(atm)[bool_short],
                                              pad_short,
                                              "left")

  bool_long <- names(atm) %in% headers_long
  names(atm)[bool_long] <-  stringr::str_pad(names(atm)[bool_long],
                                             pad_short,
                                             "left")

  atm
  #sprintf("%s\n%s",
  #        paste0(names(atm), collapse = ""),

}
