#' Helper Function: Read Meta General
#'
#' @param content_general lines with content general (see example)
#'
#' @return tibble with columns description, modelstart_datetime and unit L,T,M
#' @export
#' @importFrom stringr str_trim str_remove_all str_split_fixed
#' @importFrom tibble tibble
#' @examples
#' path_tlevel <- system.file("extdata/model/test/T_LEVEL.out", package = "kwb.hydrus1d")
#' content <- readLines(path_tlevel)
#' content_general <- content[3:5]
#' cat(content_general)
#' read_meta_general(content_general)
read_meta_general <- function(content_general) {

  units <- stringr::str_split_fixed(stringr::str_remove_all(content_general[3],
                                                            "\\s|Units|L|:|T|M|="),
                                    pattern = ",",
                                    n = 3)

  units_list <- list("L" = units[1],
                     "T" = units[2],
                     "M" = units[3])


  tibble::tibble(description = stringr::str_trim(content_general[1]),
                 modelstart_datetime = stringr::str_remove_all(content_general[2],
                                                               "Date:|\\s") %>%
                   stringr::str_replace("Time:", " "),
                 unit_L = units_list$L,
                 unit_T = units_list$T,
                 unit_M = units_list$M)
}

