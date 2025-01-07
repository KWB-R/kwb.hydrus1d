#' Read PROFILE.DAT
#'
#' @param path path to PROFILE.DAT
#'
#' @return list with sublists with material properties, profile and observations
#' data of PROFILE.DAT data
#' @export
#' @importFrom stringr str_replace
#' @importFrom stats median
read_profile <- function(path) {

  lines <- readLines(path)

  pcp_idx <- grep("Pcp_File_Version", lines)

  header_idx <- grep("x", lines)

  number_of_materials <- as.integer(lines[pcp_idx+1])

  mat_props <- lines[(pcp_idx+2):(header_idx-1)] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s{1,20}", ",") %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.data.frame()

  mat_props <- lapply(mat_props, as.numeric) %>%  dplyr::bind_rows()
  names(mat_props) <- c("mat_id", "mat_depth", "mat_prop3", "mat_prop4")


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


  obsnodes <- list(n = 0,
                   ids = NULL)

  if (length(which(ncols == 2)) > 0) {
    idx_obsnodes <- which(ncols == 2)
    obsnodes$n <- as.integer(dat[idx_obsnodes[1]])

    if(stringr::str_length(dat[idx_obsnodes[1]+1]) > 0) {

    obsnodes$ids <- stringr::str_split(dat[idx_obsnodes[1]+1],
                                       pattern = ",",
                                       simplify = TRUE) %>%
      as.integer()

    } else {
      stop("Check stucture of input file %s. Missing ids for %d specified observation points!",
           path,
           obsnodes$n)
    }
  }

  c(paste0(header_clean, collapse = ","),
    dat[ncols == median(ncols)]) %>%
    writeLines(path_profile)

  list(mat_props = mat_props,
       profile = readr::read_csv(path_profile),
       obsnodes = obsnodes)


}

