# stop_formatted ---------------------------------------------------------------
stop_formatted <- function(x, ..., call. = FALSE)
{
  stop(sprintf(x, ...), call. = call.)
}


to_r_truefalse <- function(txt) {

  txt <- stringr::str_replace_all(txt, pattern = "^t$", replacement = "TRUE")
  txt <- stringr::str_replace_all(txt, pattern = "^f$", replacement = "FALSE")
  txt
}

to_fortran_truefalse <- function(txt) {

  txt <- stringr::str_replace_all(txt, pattern = "^TRUE$", replacement = "t")
  txt <- stringr::str_replace_all(txt, pattern = "^FALSE$", replacement = "f")
  txt
}


