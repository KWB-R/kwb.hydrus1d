#' Set Metadata Attributes
#'
#' @param x R object
#' @param meta_general general metadata
#' @param meta_units information about units
#' @return \code{x} with attributes "meta_general" and "meta_units" being set
#'   (may be \code{NULL})
set_metadata <- function(x, meta_general, meta_units)
{
  structure(
    x,
    meta_general = meta_general,
    meta_units = meta_units
  )
}
