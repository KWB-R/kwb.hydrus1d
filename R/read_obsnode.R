#' Read Obs_Node.out
#'
#' @param path path to Obs_Node.out
#' @param to_longer convert table to longer format (default: TRUE)
#' @importFrom stringr str_trim str_split str_replace_all str_remove
#' @importFrom readr read_csv
#' @export
read_obsnode <- function(path, to_longer = TRUE) {
  # Lese die Datei ein
  lines <- readLines(path)

  lines <- lines[seq_len(grep("end", lines)-1)]

  # Lese die Node-Namen aus Zeile 9 ein
  nodes_idx <- grep("Node", lines)
  nodes_line <- lines[nodes_idx]

  node_names <- nodes_line %>%
    stringr::str_trim() %>%
    stringr::str_split("\\s{2,}", simplify = TRUE) %>%
    as.vector() %>%
    stringr::str_replace_all(pattern = "\\(\\s?", replacement = "") %>%
    stringr::str_remove("\\)") %>%
    tolower

  timeseries_idx <- grep("time", lines)

  headers <- lines[timeseries_idx] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", ",") %>%
    stringr::str_split(pattern = ",", simplify = TRUE) %>%
    as.vector() %>%
    tolower()

  block_start <- grep(pattern = "^h$", headers)
  block_end <- c(block_start[2:length(block_start)] - 1, length(headers))

  n_blocks <- length(block_start)

  headers_clean <- c("time", lapply(seq_len(n_blocks), function(i) {

    headers_sel <- headers[block_start[i]:block_end[i]]
    is_conc <- grepl("conc", headers_sel)

    if(sum(is_conc) > 0) {
      headers_sel[is_conc] <- sprintf("%s%d",
                                      headers_sel[is_conc],
                                      seq_len(sum(is_conc)))
    }

    sprintf("%s_%s", node_names[i], headers_sel)
  }) %>% unlist())


  dat_csv <- c(paste0(headers_clean, collapse = " "),
               lines[(timeseries_idx+1):length(lines)]) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", ",")

  path_csv <- file.path(tempdir(), "obs_node.csv")

  writeLines(dat_csv, path_csv)

  dat <- readr::read_csv(path_csv)

  if(to_longer) {
    dat %>%
      tidyr::pivot_longer( - time) %>%
      tidyr::separate(col = "name", into = c("node", "variable"), sep = "_")
  } else {
    dat
  }

}
