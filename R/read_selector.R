#' Read SELECTOR.in
#'
#' @param path path to SELECTOR.in
#' @export

read_selector <- function(path) {

  lines <- readLines(path)

  clean_line <- function(line, pattern = "\\s+") {
    line %>%
      stringr::str_trim() %>%
      stringr::str_split(pattern, simplify = TRUE) %>%
      as.vector()
  }

  header_values_to_list <- function (headers, values) {

    lapply(values, function(value) {

      is_num_val <- !is.na(suppressWarnings(as.numeric(value)))
      if(is_num_val) {
        as.numeric(value)
      } else {
        value
      }
    }) %>%
      stats::setNames(headers)
  }

  blocks_idx_start <- grep("BLOCK", lines)
  end_idx <- grep("END OF INPUT FILE 'SELECTOR.IN'", lines)

  blocks_idx_end <- c(blocks_idx_start[seq_len(length(blocks_idx_start)-1)+1] - 1,
                      end_idx - 1)


  blocks_title_start <- lines[blocks_idx_start]
  blocks_title_start_clean <- blocks_title_start %>%
    stringr::str_remove_all("\\*|BLOCK") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_replace_all(":", "_") %>%
    stringr::str_remove_all("INFORMATION")

  blocks <- tibble::tibble(name_clean = blocks_title_start_clean,
                           name_org = blocks_title_start,
                           start_idx = blocks_idx_start + 1,
                           end_ix = blocks_idx_end)


  "Pcp_File_Version=4"

  block_time <- blocks[blocks$name_clean == "C_TIME",]
  block_time_txt <- lines[block_time$start_idx:block_time$end_ix]

  time <- c(
    general = list(lapply(c(1,3,5), function(i) {
    header_values_to_list(headers = clean_line(block_time_txt[i]),
                          values = clean_line(block_time_txt[i + 1]))
    })),
    "TPrint" = list(lapply((grep("TPrint", block_time_txt)+1):length(block_time_txt),
           function(i) {
           clean_line(block_time_txt[i])
           }
    ) %>% unlist() %>% as.double()))


  block_solute <- blocks[blocks$name_clean == "F_SOLUTETRANSPORT",]
  block_solute_txt <- lines[block_solute$start_idx: block_solute $end_ix]

  header_val_idx <- grep("Epsi|iNonEqul|kTopSolute|tPulse", block_solute_txt)
  solute_transport_idx <- grep("Bulk.d.", block_solute_txt)
  solute_reaction_idx <- grep("DifW", block_solute_txt)


solute_transport <- list(transport =
  lapply((solute_transport_idx+1):(min(solute_reaction_idx)-1), function(i) {
  vec <- clean_line(block_solute_txt[i],  pattern = "\\s{2,}") %>% as.numeric()
  names(vec) <- clean_line(block_solute_txt[solute_transport_idx],  pattern = "\\s{2,}")
  vec
}) %>% dplyr::bind_rows())


solute_reaction <- list(reaction = stats::setNames(lapply(solute_reaction_idx, function(reac_idx) {
  reac_max_idx <- if(reac_idx == max(solute_reaction_idx)) {
    grep("kTopSolute", block_solute_txt) - 1
  } else {
    solute_reaction_idx[which(solute_reaction_idx == reac_idx)+1]-1
  }

   list(header_values_to_list(headers =  clean_line(block_solute_txt[reac_idx])[1:2],
                                 values = clean_line(block_solute_txt[reac_idx+1])),
           lapply((reac_idx+3):reac_max_idx, function(i) {
             vec <- clean_line(block_solute_txt[i],
                               pattern = "\\s{2,}") %>%
               as.numeric()
             names(vec) <- clean_line(block_solute_txt[reac_idx+2],
                                      pattern = "\\s{2,}")
             vec
             }) %>%
             dplyr::bind_rows()
    )}),
 nm = sprintf("solute_%d", seq_along(solute_reaction_idx))
 )
)




solute <- c(general_1 = list(lapply(header_val_idx[1:2], function(i) {
header_values_to_list(headers = clean_line(block_solute_txt[i]),
                      values = clean_line(block_solute_txt[i + 1]))
})),
solute_transport,
solute_reaction,
general_2 = list(lapply(header_val_idx[3:4], function(i) {
  header_values_to_list(headers = clean_line(block_solute_txt[i]),
                        values = clean_line(block_solute_txt[i + 1]))
})))

list(time = time,
     solute = solute)

}

