#' Read BALANCE.out
#'
#' @description Reads BALANCE.out output
#'
#' @param path full path to BALANCE.out file (default: system.file("extdata/model/test/BALANCE.out",
#' package = "kwb.hydrus1d"))
#' @return tibble with "balance" time series
#' run
#' @importFrom stringr str_extract str_remove_all str_split_fixed str_trim
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom tidyselect starts_with
#' @importFrom tidyr pivot_longer
#' @export
read_balance <- function (path =  system.file("extdata/model/test/BALANCE.out",
                                              package = "kwb.hydrus1d")) {

lines <- readLines(path)

#meta <- read_meta_general(content_general = lines[3:5])

calctime_seconds <- lines[grep("Calculation time", lines)] %>%
  stringr::str_extract("\\d+?\\.\\d+") %>%
  as.double()

#dplyr::bind_cols(tibble::tibble("calculationtime_seconds" = calctime_seconds))


block_start <- grep("^ Time", lines)
block_end <- c(block_start[2:length(block_start)]-4, length(lines)-3)

blocks <- tibble::tibble(id = seq_len(length(block_start)),
                         start_idx = block_start,
                         end_idx = block_end)


budget <- lapply(seq_len(nrow(blocks)), function(i) {

block_sel_txt <- lines[blocks$start_idx[i]:blocks$end_idx[i]]


time <- stringr::str_extract(block_sel_txt[1], "\\d+?\\.\\d+") %>%
  as.double()
subregion_ids <-  c(0, stringr::str_extract_all(block_sel_txt[3], "\\d+?")[[1]] %>%
  as.integer())

bal_id_start <- grep("Length", block_sel_txt)
bal_id_end <- length(block_sel_txt)


lapply(block_sel_txt[bal_id_start:bal_id_end], function(x) {
  stringr::str_extract_all(x, "\\d+?\\.\\d+")[[1]] %>% as.double() %>% t() %>%
    tibble::as_tibble()
}) %>%
  dplyr::bind_rows()

names(balance) <- sprintf("id_%d", subregion_ids)


parvals <- block_sel_txt[bal_id_start:bal_id_end] %>%
  stringr::str_remove_all("-?\\d+?\\.\\d+E\\+?-?\\d\\d") %>%
  stringr::str_trim()

parvals_df <- parvals %>%
  stringr::str_split_fixed("\\[.*\\]", n = 3) %>%
  tibble::as_tibble()

names(parvals_df) <- c("parameter", "unit", "solute_id")

parvals_df$parameter <- parvals_df$parameter %>% stringr::str_trim()
parvals_df$solute_id <- as.integer(parvals_df$unit)
parvals_df$unit <- stringr::str_extract(parvals, "\\[(.*)\\]") %>%
  stringr::str_remove_all("\\[|\\]") %>%
  stringr::str_trim()



dplyr::bind_cols(tibble::tibble(time = time,
                                parvals_df), balance)
}) %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_longer(cols = tidyselect::starts_with("id_"),
                      names_to = "subdomain_id") %>%
  dplyr::mutate(subdomain_id = stringr::str_remove(subdomain_id,
                                                   "id_") %>%
                  as.integer())


}
