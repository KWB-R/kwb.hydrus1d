#' Read ATMOSPH.in
#'
#' @param path path to ATMOSPH.in (default: system.file("extdata/model/test/ATMOSPH.in",
#' package = "kwb.hydrus1d"))
#'
#' @return list with atmospheric config parameter sublist and time series sublist
#' @export
#' @importFrom readr read_table
#' @importFrom stringr str_remove str_trim str_split
#' @importFrom stats setNames
#'
read_atmosph <- function(path = system.file("extdata/model/test/ATMOSPH.in",
                                            package = "kwb.hydrus1d")) {

atm <- readLines(path)

idx_headers <- grep("\\s+tAtm\\s+", atm)
idx_start <- idx_headers + 1
idx_end <- grep("end\\*\\*\\*", atm) - 1

idx_config_start <- grep("ATMOSPHERIC INFORMATION", atm) + 1
idx_config_end <- idx_headers - 1

config_names_vars <- lapply(1:6, function(i) {
  atm[idx_config_start:idx_config_end][i] %>%
    stringr::str_remove("\\(MaxAL = number of atmospheric data-records\\)") %>%
    stringr::str_remove("\\(max. allowed pressure head at the soil surface\\)") %>%
    stringr::str_trim(side = "both") %>%
    stringr::str_split("\\s+", simplify = TRUE) %>%
    as.vector()
})


atm_config <- stats::setNames(sapply(c(config_names_vars[[2]], config_names_vars[[4]], config_names_vars[[6]]),
                                     list),
                              nm = c(config_names_vars[[1]], config_names_vars[[3]], config_names_vars[[5]]))


atm_names <-  stringr::str_trim(atm[idx_headers], side = "both") %>%
  stringr::str_split("\\s+", simplify = TRUE) %>%
  as.vector()

a_file <- tempfile()

atm[idx_start:idx_end] %>%
  stringr::str_trim(side = "both") %>%
  writeLines(a_file)

atm_dat <- readr::read_table(a_file, col_names = FALSE)
names(atm_dat)[seq_along(atm_names)] <- atm_names

list(config = atm_config,
     data = atm_dat)
}
