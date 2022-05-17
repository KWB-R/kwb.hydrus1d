#' Helper function: get units list from meta general
#'
#' @param meta_general as retrieved by \code{read_meta_general}
#' @return list with elements unit_L, unit_T and unit_M
#' @importFrom tidyselect starts_with
#' @export
#' @examples
#' path_tlevel <- system.file("extdata/model/test/T_LEVEL.out", package = "kwb.hydrus1d")
#' content <- readLines(path_tlevel)
#' content_general <- content[3:5]
#' cat(content_general)
#' meta_general <- read_meta_general(content_general)
#' get_units_list(meta_general)
#'
get_units_list <- function(meta_general) {

  meta_general %>%
    dplyr::select(tidyselect::starts_with("unit")) %>%
    as.list()
}
