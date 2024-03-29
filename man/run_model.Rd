% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_model.R
\name{run_model}
\alias{run_model}
\title{Run Model}
\usage{
run_model(
  exe_path = check_hydrus_exe(),
  model_path = system.file("extdata/model/test", package = "kwb.hydrus1d"),
  ...
)
}
\arguments{
\item{exe_path}{path to Hydrus1D executable v4.17.040 (default: as retrieved
by \code{check_hydrus_exe})}

\item{model_path}{path to model directory (default: system.file("extdata/model/test",
package = "kwb.hydrus1d"))}

\item{...}{additional arguments passed to \code{shell}}
}
\value{
runs HYDRUS 1D model
}
\description{
Run Model
}
\examples{
run_model()
}
