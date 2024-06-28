#' Read HYDRUS1D.dat file
#'
#' @param path path to HYDRUS1D.dat file (default: system.file("extdata/model/test/HYDRUS1D.dat",
#' package = "kwb.hydrus1d"))
#'
#' @return list with parameters defined in
#' @export
#' @importFrom stringr str_remove_all

read_hydrus1d <- function(path = system.file("extdata/model/test/HYDRUS1D.dat",
                                             package = "kwb.hydrus1d")) {


  lines <- readLines(path)

  section_start_idx <- which(stringr::str_detect(lines, "\\["))

  section_end_idx <- c(max(section_start_idx) - 4, length(lines))

  section_names <- stringr::str_remove_all(lines[section_start_idx], "\\[|\\]")



  convert_to_numeric <- function(value) {
    num_value <- suppressWarnings(as.numeric(value))
    if (!is.na(num_value)) {
      return(num_value)
    } else {
      return(value)
    }
  }


  setNames(lapply(seq_len(length(section_names)), function(i) {

    mat_data <- stringr::str_split_fixed(lines[(section_start_idx[i]+1):section_end_idx[i]], pattern = "=", n = 2)

    setNames(lapply(mat_data[, 2], convert_to_numeric), mat_data[, 1])
  }), nm = section_names)


}


#' Write HYDRUS1D.dat
#'
#' @param hydrus1d_list as retrieved by \code{read_hydrus1d}
#' @param path path to HYDRUS1D.dat for export (default: system.file("extdata/model/test/HYDRUS1D.dat",
#' package = "kwb.hydrus1d")
#'
#' @return write HYDRUS1D.dat
#' @export
#'
write_hydrus1d <- function(hydrus1d_list,
                           path = system.file("extdata/model/test/HYDRUS1D.dat",
                                       package = "kwb.hydrus1d")) {


  unlisted_section1 <- unlist(hydrus1d_list[[1]])
  unlisted_section2 <- unlist(hydrus1d_list[[2]])


  lines <- c(";",
             sprintf("[%s]", names(hydrus1d_list)[1]),
             sprintf("%s=%s", names(unlisted_section1), as.character(unlisted_section1)),
             ";",
             sprintf("%s=%s", names(unlisted_section2), as.character(unlisted_section2)))

  writeLines(lines, path)

}
