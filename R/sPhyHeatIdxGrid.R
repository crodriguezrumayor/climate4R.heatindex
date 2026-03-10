#'
#' @title Simplified Physiological Heat Index in climate4R
#' @description Computation of the simplified physiological heat index introduced by Lu et al. (2025) directly 
#' from climate4R objects.
#'
#' @param tas A climate4R dataset of air temperature.
#' @param hurs A climate4R dataset of relative humidity.
#' @param parallel Logical. Enable parallel processing (default = FALSE).
#' @param max.ncores Integer. Maximum number of cores to use (default = 16).
#' @param ncores Integer. Specific number of cores to use.
#'
#' @return A climate4R object with the computed index. 
#' @details Air temperature is internally converted to Kelvin and relative humidity to 0-1 fraction.
#' The vectorised C++ backend \code{heatindex_vec} of the \pkg{heatindex} package (Lu and Romps, 2025)
#' is internally called.
#'
#' @references
#' Lu, Y.-C. and Romps, D. M. (2025). heatindex: Tools for Calculating Heat Stress. \url{https://heatindex.org}.
#'  
#' Lu, Y.-C., et al. (2025). Simpler and faster: An improved heat index. Journal of Applied Meteorology 
#' and Climatology, in review.
#'
#' @import transformeR
#' @importFrom magrittr %>% %<>% extract2
#' @importFrom convertR udConvertGrid
#' @importFrom udunits2 ud.are.convertible ud.convert
#' @importFrom parallel stopCluster
#' @import heatindex
#' 
#' @examples \dontrun{
#' library(climate4R.heatindex)
#'
#' data("ERA5_day_t2m", package = "climate4R.heatindex")
#' data("ERA5_day_hurs", package = "climate4R.heatindex")
#'
#' sphi <- heatindexGrid(tas = ERA5_day_t2m, hurs = ERA5_day_hurs)
#' }
#' 
#' @author climate4R adaptation by C. Rodriguez-Rumayor.
#' Original \pkg{heatindex} by Yi-Chuan Lu & David M. Romps \url{https://heatindex.org}.
#' @export

heatindexGrid <- function(tas,
                            hurs,
                            parallel = FALSE,
                            max.ncores = 16,
                            ncores  = NULL) {
                                
    if (is.null(tas) || is.null(hurs)) {
        stop("Both tas and hurs inputs are required for heat index calculation.")
    }

    # Basic object validation
    if (isMultigrid(tas) || isMultigrid(hurs)) {
        stop("Multigrids are not an allowed input")
    }
    stopifnot(isGrid(tas), isGrid(hurs))

    # Convert inputs to required units
    tas.u <- getGridUnits(tas)
    if (ud.are.convertible(tas.u, "K")) {
        if (ud.convert(1, tas.u, "K") != 1) {
            message("[", Sys.time(), "] Converting air temperature units ...")
            tas %<>% udConvertGrid(new.units = "K")
        }
    } else {
        stop("Non compliant tas units (", tas.u, " is not convertible to Kelvin)")
    }

    hurs.u <- getGridUnits(hurs)
    if (tolower(hurs.u) %in% "percentage") {
        attr(hurs$Variable, "units") <- "%"
        hurs.u <- "%"
    }
    if (ud.are.convertible(hurs.u, "%")) {
        if (ud.convert(1, hurs.u, "%") != 1) {
            message("[", Sys.time(), "] Converting relative humidity units ...")
            hurs %<>% udConvertGrid(new.units = "%")
        }
    } else {
        stop("Non compliant hurs units (", hurs.u, " is not convertible to %)")
    }

    # Sanity check on hurs values
    if (any(hurs$Data < 0 | hurs$Data > 100, na.rm = TRUE)) {
        stop("Some relative humidity values are outside the expected [0, 100] range")
    }

    # Convert hurs to 0-1 fraction
    hurs$Data <- hurs$Data / 100

    # Ensure member dimension 
    tas  %<>% redim(member = TRUE)
    hurs %<>% redim(member = TRUE)

    # Consistency checks
    if (typeofGrid(tas) != typeofGrid(hurs)) {
        stop("Input variables must be of the same type (either grid or station).")
    }
    suppressMessages(checkDim(tas, hurs, dimensions = c("time", "lat", "lon")))

    station <- typeofGrid(tas) == "station"
    n.mem   <- getShape(tas, "member")

    # Set up parallel processing
    if (n.mem > 1) {
        parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
        apply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "lapply")
        if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
    } else {
        if (isTRUE(parallel)) message("NOTE: Parallel processing was skipped (unable to parallelize one single member)")
        apply_fun <- lapply
    }

    message("[", Sys.time(), "] Calculating heat index ...")

    # Process each member separately 
    out.list <- apply_fun(seq_len(n.mem), function(m) {

        tas_m <- subsetGrid(tas,  members = m, drop = TRUE) %>% redim(member = FALSE) %>% extract2("Data")
        hurs_m <- subsetGrid(hurs, members = m, drop = TRUE) %>% redim(member = FALSE) %>% extract2("Data")

        # Call the C++ vectorised backend directly
        aux <- heatindex:::heatindex_vec(tas_m, hurs_m)
        dim(aux) <- dim(tas_m)

        # Build output grid
        out.grid <- subsetGrid(tas, members = m, drop = FALSE)
        out.grid$Data <- aux
        attr(out.grid$Data, "dimensions") <- attr(tas_m, "dimensions")

        return(out.grid)
    })

    # Combine members 
    out <- if (length(out.list) == 1) {
        out.list[[1]]
    } else {
        do.call(bindGrid, c(out.list, list(dimension = "member")))
    }

    # Final redim for station objects
    if (station) {
        out$Data <- drop(out$Data)
        attr(out$Data, "dimensions") <- c("time", "loc")
    } else {
        out$Data <- drop(out$Data)
        attr(out$Data, "dimensions") <- c("time", "lat", "lon")
    }

    # Update variable metadata
    tas <- hurs <- NULL
    out$Variable$varName <- "sphi"
    attr(out$Variable, "units") <- "K"
    attr(out$Variable, "longname") <- "Simplified Physiological Heat Index"

    message("[", Sys.time(), "] Done.")
    invisible(out)

}