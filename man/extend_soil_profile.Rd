% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extend_soil_profile.R
\name{extend_soil_profile}
\alias{extend_soil_profile}
\title{Extend soil profile}
\usage{
extend_soil_profile(df, x_end)
}
\arguments{
\item{df}{data frame with soil profile. As retrieved by \code{read_profile}
and go to sublist "profile"}

\item{x_end}{maximum soil depth to be used for extrapolation (or reduction)}
}
\value{
extended (or reduced) soil profile
}
\description{
Extend soil profile
}
