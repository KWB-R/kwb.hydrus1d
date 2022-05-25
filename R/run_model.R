#' Run Model
#'
#' @param exe_path path to Hydrus1D executable v4.17.040 (default: as retrieved
#' by \code{check_hydrus_exe})
#' @param model_path path to model directory (default: system.file("extdata/model/test",
#' package = "kwb.hydrus1d"))
#' @param ... additional arguments passed to \code{shell}
#' @return runs HYDRUS 1D model
#' @export
#' @importFrom fs file_copy path_abs
#' @examples
#' run_model()
run_model <- function(exe_path = check_hydrus_exe(),
                      model_path = system.file("extdata/model/test", package = "kwb.hydrus1d"),
                      ...
) {
exe_name <- basename(exe_path)
target_dir <- dirname(model_path)
target_path <- file.path(target_dir, exe_name)

fs::file_copy(path = exe_path,
              new_path = fs::path_abs(target_path),
              overwrite = TRUE
)

model_dir <- fs::path_abs(sprintf("%s\n",
                                  model_path))

writeLines(model_dir,
           fs::path_abs(sprintf("%s/LEVEL_01.DIR",
                                target_dir)))
shell(cmd = sprintf("cd %s && %s",
                    fs::path_abs(target_dir),
                    exe_name),
      intern = FALSE,
      ...)
}
