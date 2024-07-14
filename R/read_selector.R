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
      } else if (value %in% c("f", "t") ) {
        to_r_truefalse(value)
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


  block_basic <- blocks[blocks$name_clean == "A_BASIC",]
  block_basic_txt <- lines[block_basic$start_idx:block_basic$end_ix]

  i1 <-grep("Heading", block_basic_txt)

  i2 <- grep("LUnit  TUnit  MUnit", block_basic_txt)

  i3 <- grep("^lWat|^lSnow|^NMat", block_basic_txt)


  config_basic <- c(header_values_to_list(headers = block_basic_txt[i1],
                         values = paste(block_basic_txt[i1 + 1], collapse = "")),
  header_values_to_list(headers = block_basic_txt[i2] %>%  stringr::str_remove_all("\\s+?\\(.*") %>%
                          stringr::str_split("\\s{1,10}", simplify = TRUE) %>%
                          as.vector(),
                        values = block_basic_txt[(i2+1):(i2+3)]),
  lapply(i3, function(i) {

    header_values_to_list(headers = clean_line(block_basic_txt[i]),
                          values = clean_line(block_basic_txt[i + 1]))
  }) %>%
    unlist() %>%
    as.list()
  )



  block_water <- blocks[blocks$name_clean == "B_WATERFLOW",]
  block_water_txt <- lines[block_water$start_idx:block_water$end_ix]

  i1 <-grep("MaxIt", block_water_txt)
  block_water_txt[i1] <- stringr::str_remove(block_water_txt[i1], "\\s+?\\(.*")

  i2 <- grep("TopInf|BotInf|hTab1|Model", block_water_txt)

  i3 <- grep("thr", block_water_txt)

  soil_dat <- block_water_txt[i3:length(block_water_txt)] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", ",") %>%
    stringr::str_split_fixed(pattern = ",", n = 6)

  soil <- tibble::as_tibble(soil_dat[2:nrow(soil_dat),]) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), .fns = as.numeric))

  names(soil) <- soil_dat[1,]

  config_water <- c(
    lapply(c(i1, i2), function(i) {
      header_values_to_list(headers = clean_line(block_water_txt[i]),
                            values = clean_line(block_water_txt[i + 1]))
    }) %>% unlist() %>% as.list(),
    list(soil = soil))


  block_time <- blocks[blocks$name_clean == "C_TIME",]
  block_time_txt <- lines[block_time$start_idx:block_time$end_ix]

  config_time <- c(
    lapply(c(1,3,5), function(i) {
    header_values_to_list(headers = clean_line(block_time_txt[i]),
                          values = clean_line(block_time_txt[i + 1]))
    }) %>% unlist() %>% as.list(),
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


solute_reaction <- stats::setNames(lapply(solute_reaction_idx, function(reac_idx) {
  reac_max_idx <- if(reac_idx == max(solute_reaction_idx)) {
    grep("kTopSolute", block_solute_txt) - 1
  } else {
    solute_reaction_idx[which(solute_reaction_idx == reac_idx)+1]-1
  }

   list(diffusion = header_values_to_list(headers =  clean_line(block_solute_txt[reac_idx])[1:2],
                                 values = clean_line(block_solute_txt[reac_idx+1])) %>%
          unlist() %>% t() %>% tibble::as_tibble(),
        reaction = lapply((reac_idx+3):reac_max_idx, function(i) {
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

general_1 = lapply(header_val_idx[1:2], function(i) {
  header_values_to_list(headers = clean_line(block_solute_txt[i]),
                        values = clean_line(block_solute_txt[i + 1]))
}) %>% unlist() %>% as.list()


general_2 = lapply(header_val_idx[3:4], function(i) {
  header_values_to_list(headers = clean_line(block_solute_txt[i]),
                        values = clean_line(block_solute_txt[i + 1]))
}) %>% unlist() %>% as.list()


gen2_is_na <- is.na(names(general_2))

names(general_2)[gen2_is_na] <- sprintf("unknown%02d", seq_len(sum(gen2_is_na)))


config_solute <- c(general_1,
solute_transport,
solute_reaction,
general_2
)

list(basic = config_basic,
     time = config_time,
     waterflow = config_water,
     solute = config_solute)

}

