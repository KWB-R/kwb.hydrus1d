#' Convert Atmosphere to String
#'
#' @param atm  atm as retrieved by \code{prepare_atmosphere_data}
#' @param round_digits digits used for rounding values (default: 2) of all columns
#' besides "tAtm"
#' @param remove_scientific if TRUE scientific notation of numbers is removed,
#' otherwise not (default: TRUE)
#' @return properly padded string of atmosphere variables for Hydrus1D input file
#' @export
#' @importFrom stringr str_pad
convert_atmosphere_to_string <- function(atm,
                                         round_digits = 2,
                                         remove_scientific = TRUE) {


  if(remove_scientific) {
  ### Get default options
  opts.default <- options()

  ### get rid of scientific notation
  options(scipen = 999)

  # Reset to defaults:
  options(opts.default)
}

  pad_short <- 11
  pad_long <- 12

  headers_short <- "tAtm"
  headers_long <- get_atmosphere_headers()[!get_atmosphere_headers() %in% headers_short]



  atm[headers_long] <- (lapply(atm[headers_long],
                               function(x) round(x, round_digits)))


  atm[headers_short] <- (lapply(atm[headers_short],
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

txt <-  sprintf("%s\n%s",
          paste0(names(atm), collapse = ""),
          paste0(apply(atm, 1, function(x) paste0(x, collapse = "")), collapse = "\n"))



}
