#' Write "ATMOSPH.IN" input file
#'
#' @param atm tibble of input data as defined in \code{prepare_atmospherice_input}
#' @param MaxAL Number of meteorological records (default: nrow(atm))
#' @param DailyVar TRUE if HYDRUS-1D is to generate daily variations in evaporation
#' and transpiration (see section 2.7.2.)., otherwise: FALSE (default: FALSE)
#' @param SinusVar TRUE if HYDRUS-1D is to generate sinusoidal variations in
#' precipitation (see section 2.7.2.), otherwise: FALSE(default: FALSE)
#' @param lLai Logical variable indicating that potential evapotranspiration is
#' to be divided into potential evaporation and potential transpiration using
#' eq. (2.75). (default: FALSE)
#' @param lBCCycles TRUE if a set of boundary conditions is to be repeated
#'  multiple times, otherwise FALSE(default: FALSE)
#' @param lInterc TRUE if interception is considered using eq. (2.78), otherwise
#' FALSE (default: FALSE)
#' @param hCritS Maximum allowed pressure head at the soil surface (L). (default: 0)
#' @param round_digits digits used for rounding values (default: 2) of all columns
#' besides "tAtm"
#' @param remove_scientific if TRUE scientific notation of numbers is removed,
#' otherwise not (default: TRUE)
#' @return Creates ATMOSPH.IN input textfile
#' @importFrom kwb.utils resolve
#' @importFrom stringr str_pad
#' @export
#' @examples
#' inputs <- tibble::tibble(tAtm = 1:10, Prec = 10, rSoil = 0.4)
#' atm <- prepare_atmosphere_input(inputs)
#' atm
#' atm_string <- write_atmosphere(atm = atm, MaxAL = nrow(atm))
#' cat(atm_string)

write_atmosphere <- function (
  atm,
  MaxAL = nrow(atm),
  DailyVar = FALSE,
  SinusVar = FALSE,
  lLai = FALSE,
  lBCCycles = FALSE,
  lInterc = FALSE,
  hCritS = 0,
  round_digits = 2,
  remove_scientific = TRUE
)
{
  collapse <- function(x) paste(x, collapse = "")

  grammar <- list(
    input_file = "<A><B><C>",
    A = "<A1><A2><A3><A4><A5><A6><A7><A8>",
    A1 = "Pcp_File_Version=4\n",
    A2 = "*** BLOCK I: ATMOSPHERIC INFORMATION  **********************************\n",
    A3 = "   MaxAL                    (MaxAL = number of atmospheric data-records)\n",
    A4 = sprintf("%7d\n", MaxAL),
    A5 = " DailyVar  SinusVar  lLay  lBCCycles lInterc lDummy  lDummy  lDummy  lDummy  lDummy\n",
    A6 = sprintf(
      collapse(c(
        rep(stringr::str_pad("%s", width = 8L, "left"), 5L),
        "%s\n"
      )),
      DailyVar,
      SinusVar,
      lLai,
      lBCCycles,
      lInterc,
      collapse(rep(stringr::str_pad("f", width = 8L, side = "left"), 5L))
    ),
    A7 = " hCritS                 (max. allowed pressure head at the soil surface)\n",
    A8 = stringr::str_pad(
      sprintf("%s\n", round(hCritS, round_digits)),
      width = 7L,
      "left"
    ),
    B = "<B1>\n",
    B1 =  convert_atmosphere_to_string(atm, round_digits, remove_scientific),
    C = "<endOfFile>",
    endOfFile = "end*** END OF INPUT FILE 'ATMOSPH.IN' **********************************"
  )

  kwb.utils::multiSubstitute(
    strings = kwb.utils::resolve("input_file", grammar),
    replacements = list(
      "TRUE" = "t",
      "FALSE" = "f"
    )
  )
}

