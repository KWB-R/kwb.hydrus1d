#' Write "ATMOSPH.IN" input file
#'
#' @param MaxAL Number of meteorological records
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
#'
#' @return Creates ATMOSPH.IN input textfile
#' @importFrom kwb.utils resolve
#' @export
#' @examples
#' write_atmosphere()

write_atmosphere <- function (MaxAL = 365,
                              DailyVar = FALSE,
                              SinusVar = FALSE,
                              lLai = FALSE,
                              lBCCycles = FALSE,
                              lInterc = FALSE,
                              hCritS = 0
) {


  grammar <- list(
    input_file = "<A><B><C>",
    A = "<A1><A2><A3><A4><A5><A6>",
    A1 = "Pcp_File_Version=4\n",
    A2 = "*** BLOCK I: ATMOSPHERIC INFORMATION  **********************************\n",
    A3 = "   MaxAL                    (MaxAL = number of atmospheric data-records)\n",
    A4 = sprintf("%7d\n", MaxAL),
    A5 = " DailyVar  SinusVar  lLay  lBCCycles lInterc lDummy  lDummy  lDummy  lDummy  lDummy\n",
    A6 = sprintf("       %s       %s       %s       %s       %s       %s\n",
                 DailyVar,
                 SinusVar,
                 lLai,
                 lBCCycles,
                 lInterc,
                 paste0(rep("        f",5), collapse = "")),
    A7 = " hCritS                 (max. allowed pressure head at the soil surface)\n",
    A8 = sprintf("%7d\n", hCritS),
    B = "<B1><B2>",
    B1 = "header\n",
    B2 = "content\n",
    C = "<endOfFile>",
    endOfFile = "end*** END OF INPUT FILE 'ATMOSPH.IN' **********************************\n"
  )


  inpTxt <- kwb.utils::resolve("input_file", grammar)

  inpTxt <- gsub(pattern = "TRUE", replacement = "t", inpTxt)
  inpTxt <- gsub(pattern = "FALSE", replacement = "f", inpTxt)
  inpTxt
}

