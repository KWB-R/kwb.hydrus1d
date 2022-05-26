#' Defaults for Atmosphere
#'
#' @param Prec Precipitation (default: 0)
#' @param rSoil Evaporation (default: 0)
#' @param rRoot Transpiration (default: 0)
#' @param hCritA (default: 100000)
#' @param rB rB (default: 0)
#' @param hB hB (default: 0)
#' @param ht ht (default: 0)
#' @param tTop tTop (default: 0)
#' @param tBot tBot (default: 0)
#' @param Ampl Ampl (default: 0)
#' @param cTop concentration of solute 1 at top (default: 0)
#' @param cBot concentration of solute 1 at bottom (default: 0)
#' @param RootDepth root depth (default: 0)
#'
#' @return tibble with defaults for atmospheric parameters
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' defaults_atmosphere()
defaults_atmosphere <- function(
  Prec = 0L,
  rSoil = 0L,
  rRoot = 0L,
  hCritA = 100000L,
  rB = 0L,
  hB = 0L,
  ht = 0L,
  tTop = 0L,
  tBot = 0L,
  Ampl = 0L,
  cTop = 0L,
  cBot = 0L,
  RootDepth = 0L
)
{
  defaults <- list(
    Prec = Prec,
    rSoil = rSoil,
    rRoot = rRoot,
    hCritA = hCritA,
    rB = rB,
    hB = hB,
    ht = ht,
    tTop = tTop,
    tBot = tBot,
    Ampl = Ampl,
    cTop = cTop,
    cBot = cBot,
    RootDepth = RootDepth
  )

  tibble::as_tibble(t(unlist(defaults)))
}
