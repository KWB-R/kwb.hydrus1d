#' Read T_LEVEL.out
#'
#' @description Stores pressure heads and fluxes on the boundaries and in the
#' root zone.
#'
#' @param path full path to T_LEVEL.out file
#'
#' @return imports T_LEVEL out with tidy column names and saves metainformation
#' in attribute 'meta'
#' @export
#' @importFrom readr read_delim read_fwf fwf_widths
#' @importFrom stringr str_remove_all str_replace str_split_fixed str_trim
#' @importFrom janitor make_clean_names
#' @importFrom kwb.utils multiSubstitute
#' @importFrom rlang .data
#' @references \url{https://www.pc-progress.com/Downloads/Pgm_Hydrus1D/HYDRUS1D-4.17.pdf#page=271}
#' @examples
#' path_tlevel <- system.file("extdata/model/test/T_LEVEL.out", package = "kwb.hydrus1d")
#' tlevel <- read_tlevel(path = path_tlevel)
#' tlevel
read_tlevel <- function(path) {

content <- readLines(path)


units <- stringr::str_split_fixed(stringr::str_remove_all(content[5],
                                                          "\\s|Units|L|:|T|M|="),
                                  pattern = ",",
                                  n = 3)

units_list <- list("L" = units[1],
                   "T" = units[2],
                   "M" = units[3])

meta_general <- tibble::tibble(description = stringr::str_trim(content[3]),
                       modelstart_datetime = stringr::str_remove_all(content[4],
                                                          "Date:|\\s") %>%
                         stringr::str_replace("Time:", " "),
                       unit_L = units_list$L,
                       unit_T = units_list$T,
                       unit_M = units_list$M)


col_names <- stringr::str_trim(content[7]) %>%
  stringr::str_split(pattern = "\\s+") %>%
  unlist() %>%
  janitor::make_clean_names()


col_units <- stringr::str_split_fixed(stringr::str_remove(content[8], "\\s+"),
                         n = length(col_names), pattern = "\\s+") %>%
  stringr::str_remove_all("\\[|\\]")



meta_units <- tibble::tibble(name = col_names,
                        unit_general = col_units,
                        unit = kwb.utils::multiSubstitute(strings = .data$unit_general,
                                                 replacements = units_list))


col_widths <- c(rep(13,19), 7, 13, 11)
sum(col_widths)
stringr::str_length(content[10])
rows_to_skip <- 9
tlevel <- readr::read_fwf(file = path,
                          skip = rows_to_skip,
                          n_max = length(content)-rows_to_skip-2,
                          readr::fwf_widths(widths = col_widths,
                                            col_names = col_names))

attr(tlevel, "meta_general") <- meta_general
attr(tlevel, "meta_units") <- meta_units

tlevel

}


#' Helper Function: Get Metadata of 'T_LEVEL.out'
#'
#' @param tlevel as retrieved by \code{read_tlevel}
#'
#' @return returns metainformation list with sublists "general" and "units"  of
#' imported T_LEVEL.out file
#' @export
#'
#' @examples
#' path_tlevel <- system.file("extdata/model/test/T_LEVEL.out", package = "kwb.hydrus1d")
#' tlevel <- read_tlevel(path = path_tlevel)
#' tlevel_get_meta(tlevel)
tlevel_get_meta <- function(tlevel) {
  list(general = attr(tlevel, "meta_general"),
       units = attr(tlevel, "meta_units")
  )
}
