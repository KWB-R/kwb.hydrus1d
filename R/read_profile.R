#' Read PROFILE.out
#'
#' @param path path to PROFILE.out
#'
#' @return tibble with PROFILE.out data
#' @export
#' @importFrom stringr str_replace
#' @importFrom stats median
read_profile <- function(path) {

  lines <- readLines(path)

  header_idx <- grep("x", lines)

  dat <- lines[(header_idx+1):length(lines)] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", ",")

  ncols <- sapply(seq_along(dat), function(i) length(gregexpr(",", dat[i])[[1]])) + 1


  header_names_file <- c("node_id", stringr::str_sub(lines[header_idx],
                                                     start = gregexpr("x", lines[header_idx])[[1]][1],
                                                     end = nchar(lines[header_idx])) %>%
                           stringr::str_trim() %>%
                           stringr::str_replace_all("\\s+", " ") %>%
                           stringr::str_split(" ", simplify = TRUE) %>%
                           as.vector() %>% tolower())

  header_clean <- if(stats::median(ncols) > length(header_names_file)) {
    string_conc <- sprintf("conc%d", seq_len(median(ncols) - length(header_names_file))+1)

    c(stringr::str_replace(header_names_file, "conc", "conc1"),
      string_conc)

  } else {
    header_names_file
  }

  header_clean

  path_profile <- file.path(tempdir(), "profile.csv")

  c(paste0(header_clean, collapse = ","),
    dat[ncols == median(ncols)]) %>%
    writeLines(path_profile)

  readr::read_csv(path_profile)
}
