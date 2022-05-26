#' Check HYDRUS Executable and download if needed
#'
#' @param dir target directory (default: file.path(system.file(package =
#'   "kwb.hydrus1d"), "extdata/hydrus1d"), in case compliant HYDRUS1D version is
#'   not available, executable will be downloaded from
#'   \url{https://github.com/mrustl/hydrus1d/archive/refs/tags/v4.17.0140.zip}
#' @param skip_preinstalled if TRUE checking of preinstalled HYDRUS-1D will be
#'   skipped and executable will be downloaded, otherwise not (the default)
#' @return path to preinstalled HYDRUS-1D executable (if compliant and parameter
#'   skip_preinstalled == FALSE), otherwise path to downloaded HYDRUS-1D
#' @importFrom kwb.utils catAndRun resolve
#' @importFrom stringr str_remove_all
#' @importFrom fs dir_create
#' @importFrom archive archive_extract
#' @export
#' @examples
#' check_hydrus_exe(skip_preinstalled = FALSE)
#' check_hydrus_exe(skip_preinstalled = TRUE)
check_hydrus_exe <- function(
  dir = file.path(system.file(package = "kwb.hydrus1d"), "extdata/hydrus1d"),
  skip_preinstalled = FALSE
)
{
  paths <- kwb.utils::resolve(list(
    owner = "mrustl",
    repo = "hydrus1d",
    release = "4.17.0140",
    date = "15.11.2018",
    hydrus_zip_url = "https://github.com/<owner>/<repo>/archive/refs/tags/v<release>.zip",
    hydrus_zip_tdir = dir,
    hydrus_dir_default = "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx",
    hydrus_meta = "Ver_H1D.txt",
    hydrus_exe = "H1D_CALC.exe",
    hydrus_default_meta = "<hydrus_dir_default>/<hydrus_meta>",
    hydrus_default_exe = "<hydrus_dir_default>/<hydrus_exe>"
  ))

  preinstalled_check <- !skip_preinstalled && all(
    sapply(c(paths$hydrus_default_exe, paths$hydrus_default_meta), file.exists)
  )

  if (preinstalled_check) {

    metainfo <- readLines(paths$hydrus_default_meta, warn = FALSE)
    metainfo <- stringr::str_remove_all(metainfo[3:4], pattern = ".*=")
    meta_version <- metainfo[1L]
    meta_date <- metainfo[2L]

    kwb.utils::catAndRun(
      sprintf(
        "Checking metadata of pre-installed HYDRUS-1D (version == '%s' and date == '%s')",
        paths$release,
        paths$date
      ),
      expr = {
        if (meta_version != paths$release || meta_date != paths$date) {
          stop_formatted(
            paste(
              "Installed version '%s  and date not identical to expected ones!",
              "expected version: %s, expected date: %s",
              collapse = "\n"
            ),
            meta_version,
            meta_date,
            paths$release,
            paths$date
          )
        }
      }
    )

    # Return the path to the executable
    return(paths$hydrus_default_exe)
  }

  fs::dir_create(paths$hydrus_zip_tdir, recurse = TRUE)

  archive::archive_extract(
    archive = paths$hydrus_zip_url,
    dir = paths$hydrus_zip_tdir,
    strip_components = 1
  )

  kwb.utils::catAndRun(
    sprintf(
      "Checking if download of HYDRUS1D executable v%s from '%s' was successful",
      paths$release,
      paths$hydrus_zip_url
    ),
    expr = {

      exe_path <- file.path(paths$hydrus_zip_tdir, paths$hydrus_exe)

      if (! file.exists(exe_path)) {
        stop_formatted("Downloaded '%s' path does not exist!", exe_path)
      }

      exe_path
    }
  )
}
