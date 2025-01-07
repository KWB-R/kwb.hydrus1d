#' Read Obs_Node.out
#'
#' @param path path to Obs_Node.out
#' @param calculate_mass should masses for all concentrations be calculated, i.e.
#' flux*conc(1-n) (default: TRUE)
#' @param to_longer convert table to longer format (default: TRUE)
#' @param debug print debug messages? (default: TRUE)
#' @return tibble with Obs_Node time series data
#' @importFrom stringr str_trim str_split str_replace_all str_remove
#' @importFrom readr read_csv
#' @export
read_obsnode <- function(path, to_longer = TRUE,
                         calculate_mass = TRUE,
                         debug = TRUE) {
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
    tolower()

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

  dat <- readr::read_csv(path_csv,
                         show_col_types = if(!debug) FALSE)


  calc_mass <- function(dat) {

    # Berechne die Massen dynamisch
    conc_cols <- names(dat)[grepl("^conc", names(dat))]
    mass_cols <- paste0("mass", seq_along(conc_cols))

    dat_mass <- dat %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(conc_cols), ~ flux * ., .names = "mass{col}"))

    names(dat_mass) <- gsub("massconc", "mass", names(dat_mass))

    dat_mass
  }

  dat_long <- dat %>%
      tidyr::pivot_longer( - time) %>%
      tidyr::separate(col = "name", into = c("node_id", "variable"), sep = "_") %>%
      dplyr::mutate(node_id = stringr::str_remove(node_id, "node") %>% as.integer())


    if(calculate_mass) {
      n_conc <- dat_long %>%
        dplyr::filter(grepl("^conc", variable)) %>%
        dplyr::pull(variable) %>%
        unique() %>%
        length()

      dat_long <- kwb.utils::catAndRun(sprintf("Calculating 'mass' for %d substance concentrations (flux*conc[%s]",
                                               n_conc,
                                               paste0(seq_len(n_conc), collapse = ",")),
                           expr = {

      dat_long %>%
      tidyr::pivot_wider(names_from = "variable") %>%
      calc_mass() %>%
      tidyr::pivot_longer(names_to = "variable",
                          - tidyselect::all_of(c("time","node_id")))

    },
    dbg = debug)
    }


  if(!to_longer) {
    dat_long <- dat_long %>%
      tidyr::pivot_wider(names_from = "variable")
  }
  dat_long

}
