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
#' @importFrom kwb.utils collapsed
convert_atmosphere_to_string <- function(
  atm,
  round_digits = 2L,
  remove_scientific = TRUE
)
{
  if (remove_scientific) {

    # Get default options
    opts.default <- options()

    # Get rid of scientific notation
    options(scipen = 999)

    # Reset options on exit
    on.exit(options(opts.default))
  }

  pad_short <- 11
  pad_long <- 12

  do_pad_short <- function(x) stringr::str_pad(x, pad_short, "left")
  do_pad_long <- function(x) stringr::str_pad(x, pad_long, "left")

  headers <- get_atmosphere_headers()

  headers_short <- "tAtm"
  headers_long <- setdiff(headers, headers_short)

  atm[headers_long] <- lapply(atm[headers_long], round, round_digits)
  atm[headers_long] <- lapply(atm[headers_long], do_pad_long)

  atm[headers_short] <- lapply(atm[headers_short], do_pad_short)

  is_short <- names(atm) %in% headers_short
  is_long <- names(atm) %in% headers_long

  names(atm)[is_short] <- do_pad_short(names(atm)[is_short])
  names(atm)[is_long] <- do_pad_long(names(atm)[is_long])

  header_text <- kwb.utils::collapsed(names(atm), "")
  body_lines <- apply(atm, 1L, kwb.utils::collapsed, "")

  kwb.utils::collapsed(c(header_text, body_lines), "\n")
}
