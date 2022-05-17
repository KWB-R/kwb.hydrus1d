#' Helper Function: Get Metadata of Outputs ('A_LEVEL.out', 'T_LEVEL.out')
#'
#' @param output imported output as retrieved by \code{read_alevel} or \code{read_tlevel}
#'
#' @return returns metainformation list with sublists "general" and "units"  of
#' imported 'A_LEVEL.out' or 'T_LEVEL.out' file
#' @export
#' @examples
#' path_alevel <- system.file("extdata/model/test/A_LEVEL.out", package = "kwb.hydrus1d")
#' alevel <- read_alevel(path = path_alevel)
#' get_output_meta(alevel)
#' path_tlevel <- system.file("extdata/model/test/T_LEVEL.out", package = "kwb.hydrus1d")
#' tlevel <- read_tlevel(path = path_tlevel)
#' get_output_meta(tlevel)
get_output_meta <- function(output) {
  list(
    general = attr(output, "meta_general"),
    units = attr(output, "meta_units")
  )
}
