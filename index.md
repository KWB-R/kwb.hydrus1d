[![R-CMD-check](https://github.com/KWB-R/kwb.hydrus1d/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.hydrus1d/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.hydrus1d/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.hydrus1d/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.hydrus1d/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.hydrus1d)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.hydrus1d)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.hydrus1d)](https://kwb-r.r-universe.dev/)

R Interface for the Last Official Release of [Hydrus1D (v4.17.0140)](https://www.pc-progress.com/en/Default.aspx?H1d-downloads) for Windows.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install kwb.hydrus1d in R
install.packages('kwb.hydrus1d')

# Browse the kwb.swmm manual pages
help(package = 'kwb.hydrus1d')
```
## Usage 

Checkout the [Workflow](articles/workflow.html) article on how to use this R package.
