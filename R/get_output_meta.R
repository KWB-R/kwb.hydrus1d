#' Helper Function: Get Metadata of Outputs ('A_LEVEL.out', 'T_LEVEL.out')
#'
#' @param output imported output as retrieved by \code{read_alevel} or \code{read_tlevel}
#'
#' @return returns metainformation list with sublists "general" and "units"  of
#' imported 'A_LEVEL.out' or 'T_LEVEL.out' file
#' @importFrom kwb.utils getAttribute
#' @export
#' @examples
#' test_file <- function(x) system.file("extdata/model/test", x, package = "kwb.hydrus1d")
#' alevel <- read_alevel(path = test_file("A_LEVEL.out"))
#' get_output_meta(alevel)
#' tlevel <- read_tlevel(path = test_file("T_LEVEL.out"))
#' get_output_meta(tlevel)
get_output_meta <- function(output) {
  list(
    general = kwb.utils::getAttribute(output, "meta_general"),
    units = kwb.utils::getAttribute(output, "meta_units")
  )
}
