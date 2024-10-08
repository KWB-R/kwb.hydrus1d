#' Run Model
#'
#' @param exe_path path to Hydrus1D executable v4.17.040 (default: as retrieved
#' by \code{check_hydrus_exe})
#' @param model_path path to model directory (default: system.file("extdata/model/test",
#' package = "kwb.hydrus1d"))
#' @param print_output if TRUE (the default) the output goes to the console, if
#' FALSE, it is returned as an R object (character vector)
#' @param ... additional arguments passed to \code{shell}
#' @return runs HYDRUS 1D model
#' @export
#' @importFrom fs file_copy path_abs
#' @examples
#' run_model()
run_model <- function(
  exe_path = check_hydrus_exe(),
  model_path = system.file("extdata/model/test", package = "kwb.hydrus1d"),
  print_output = TRUE,
  ...
)
{
  exe_name <- basename(exe_path)
  target_dir <- dirname(model_path)

  fs::file_copy(
    path = exe_path,
    new_path = fs::path_abs(file.path(target_dir, exe_name)),
    overwrite = TRUE
  )

  file <- fs::path_abs(file.path(target_dir, "LEVEL_01.DIR"))

  writeLines(c(fs::path_abs(model_path), ""), file)

  shell(
    cmd = sprintf("cd %s && %s", fs::path_abs(target_dir), exe_name),
    intern = !print_output,
    ...
  )
}
