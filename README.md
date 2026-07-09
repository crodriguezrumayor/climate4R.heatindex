# climate4R.heatindex

## What is `climate4R.heatindex`?

A R package for computing the simplified physiological heat index directly from climate data within the `climate4R` framework.

`climate4R.heatindex` is a wrapper of the [`heatindex`](https://github.com/davidromps/heatindex) R package (Lu and Romps, 2025), which implements the simpler and faster heat index introduced by Lu et al. (2026) as a vectorised C++ backend. This wrapper adapts that function for seamless integration with the **climate4R** data structures, providing support for parallel computing.

## Installation

The recommended procedure for installing the package is using the `remotes` package:

```R
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("crodriguezrumayor/climate4R.heatindex")
```

Note that the following dependencies need to be installed beforehand:

```R
remotes::install_github("SantanderMetGroup/transformeR")
remotes::install_github("SantanderMetGroup/convertR")
# heatindex package - see https://heatindex.org for installation instructions
```

## Usage

```R
library(climate4R.heatindex)

data("ERA5_day_t2m", package = "climate4R.heatindex")
data("ERA5_day_hurs", package = "climate4R.heatindex")

sphi <- heatindexGrid(tas = ERA5_day_t2m, hurs = ERA5_day_hurs)
```

## References

Lu, Y.-C. and Romps, D. M. (2025). heatindex: Tools for Calculating Heat Stress. https://heatindex.org.

Lu, Y., A. Goodman, P. Kalmus, and D. M. Romps, 2026: Simpler and Faster: An Improved Heat Index. J. Appl. Meteor. Climatol., 65, 665–679, https://doi.org/10.1175/JAMC-D-25-0067.1. 