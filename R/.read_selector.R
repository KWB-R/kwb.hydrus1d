if(FALSE) {
selector_in <- file.path(paths$model_dir, "SELECTOR.in")

selector_list <- read_selector_list(path = selector_in)

selector_list$`_BLOCK_B_WATER_FLOW_INFORMATION`

waterflow_list <- read_waterflow(selector_list$`_BLOCK_B_WATER_FLOW_INFORMATION`)
waterflow_list

as.character(write_waterflow_txt(waterflow_list))

write_selector_text(selector_list)

res_write <- res
names(res_write) <- to_orig_headers(names(res))
unlist(res_write)

}

read_waterflow <- function(txt) {
  txt <- selector_list$`_BLOCK_B_WATER_FLOW_INFORMATION`

  line_2 <- stringr::str_remove(txt[1], pattern = " \\(.*") %>%
  stringr::str_split_fixed(pattern = " ", n = 3) %>%
  as.character()

  line_3 <- stats::setNames(stringr::str_split_fixed(txt[2], pattern = " ", n = 3) %>%
    as.list() %>%
    lapply(as.numeric),
    line_2)

  line_4 <- txt[3] %>%
    stringr::str_split_fixed(pattern = " ", n = 4) %>%
    as.character()

  line_5 <- stats::setNames(stringr::str_split_fixed(txt[4], pattern = " ", n = 4) %>%
                            as.character() %>%
                            to_r_truefalse() %>%
                            as.list(),
                            line_4)

  line_6 <- txt[5] %>%
    stringr::str_split_fixed(pattern = " ", n = 7) %>%
    as.character()

  line_7 <- stats::setNames(stringr::str_split_fixed(txt[6], pattern = " ", n = 7) %>%
                              as.character() %>%
                              to_r_truefalse() %>%
                              as.list(),
                            line_6)

  line_7[[5]] <- as.integer(line_7[[5]])
  for (i in c(1:4,6)) {
    line_7[[i]] <- as.logical(line_7[[i]])
  }
  line_7[[7]] <- as.double(line_7[[7]])

  line_10 <- txt[7] %>%
    stringr::str_split_fixed(pattern = " ", n = 2) %>%
    as.character()

  line_11 <- stats::setNames(stringr::str_split_fixed(txt[8], pattern = " ", n = 2) %>%
                              as.double() %>%
                              as.list(),
                            line_10)

  line_12 <- txt[9] %>%
    stringr::str_split_fixed(pattern = " ", n = 2) %>%
    as.character()

  line_13 <- stats::setNames(stringr::str_split_fixed(txt[10], pattern = " ", n = 2) %>%
                               as.integer() %>%
                               as.list(),
                             line_12)

  list(iteration = line_3,
       line5 = line_5,
       line7 = line_7,
       line11 = line_11,
       line13 = line_13)

}

write_waterflow_txt <- function(waterflow_list) {

  collapse_strings <- function(vector) {
    vector %>%
    to_fortran_truefalse() %>%
    stringr::str_c(collapse = " ")
  }

  lines_list <- list(line2 = collapse_strings(names(waterflow_list$iteration)),
                     line3 = collapse_strings(waterflow_list$iteration),
                     line4 = collapse_strings(names(waterflow_list$line5)),
                     line5 = collapse_strings(waterflow_list$line5),
                     line6 = collapse_strings(names(waterflow_list$line7)),
                     line7 = collapse_strings(waterflow_list$line7),
                     line10 = collapse_strings(names(waterflow_list$line11)),
                     line11 = collapse_strings(waterflow_list$line11),
                     line12 = collapse_strings(names(waterflow_list$line13)),
                     line13 = collapse_strings(waterflow_list$line13))

  unlist(lines_list)

}

read_selector_list <- function(path)  {

selector <- readLines(path)

hydrus_version_headerline <- "Pcp_File_Version=4"

stopifnot(identical(selector[1], hydrus_version_headerline))


res <- kwb.utils::extractRowRanges(selector, pattern = "\\*\\*\\*",
                                   nameByMatch = TRUE) %>%
  lapply(., kwb.utils::hsTrim)


res[!names(res) %in% "_END_OF_INPUT_FILE_SELECTOR_IN"]

}


to_orig_headers <- function(header_names) {
  header_names %>%
  stringr::str_replace("_", "*** ") %>%
  stringr::str_replace(pattern = "_", " ") %>%
  stringr::str_replace(pattern = "_", ": ") %>%
  stringr::str_replace_all("_", " ") %>%
  stringr::str_replace("(A-Z)$", " ") %>%
  sprintf("%s ", .) %>%
  stringr::str_pad(width = 72, side = "right", pad = "*")
}


end_of_input_file <- function() {
  stringr::str_pad("*** END OF INPUT FILE 'SELECTOR.IN' ",
                                      width = 72,
                                      side = "right",
                                      pad = "*")
}


write_selector_text <- function(selector_list) {

  c("Pcp_File_Version=4",
    unlist(sapply(seq_len(length(selector_list)), function(i) {
      c(to_orig_headers(names(selector_list))[i],
        selector_list[[i]])
    })),
    end_of_input_file())
}
