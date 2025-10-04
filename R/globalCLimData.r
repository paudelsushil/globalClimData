#' Download Global Climate Data
#'
#' A unified interface to download climate data from multiple sources.
#' This function routes to the appropriate data source function based on
#' the specified source parameter.
#'
#' @param source Character string specifying the data source. 
#'   Options: "TerraClimate" or "GridMET"
#' @param years Numeric vector of years to download
#'   \itemize{
#'     \item TerraClimate: must be >= 1958
#'     \item GridMET: must be >= 1979
#'   }
#' @param variable Character string specifying the climate variable to download.
#'   Available variables depend on the data source:
#'   
#'   \strong{TerraClimate variables:}
#'   \itemize{
#'     \item \code{aet} - Actual Evapotranspiration
#'     \item \code{def} - Climate Water Deficit
#'     \item \code{pet} - Potential Evapotranspiration
#'     \item \code{ppt} - Precipitation
#'     \item \code{q} - Runoff
#'     \item \code{soil} - Soil Moisture
#'     \item \code{srad} - Downward Surface Shortwave Radiation
#'     \item \code{swe} - Snow Water Equivalent
#'     \item \code{tmax} - Maximum Temperature
#'     \item \code{tmin} - Minimum Temperature
#'     \item \code{vap} - Vapor Pressure
#'     \item \code{ws} - Wind Speed
#'     \item \code{vpd} - Vapor Pressure Deficit
#'     \item \code{PDSI} - Palmer Drought Severity Index
#'   }
#'   
#'   \strong{GridMET variables:}
#'   \itemize{
#'     \item \code{bi} - Burning Index (BI) - model-G
#'     \item \code{erc} - Energy Release Component (ERC) - model-G
#'     \item \code{etr} - Reference alfalfa evapotranspiration
#'     \item \code{pet} - Reference grass evapotranspiration
#'     \item \code{pr} - Precipitation amount
#'     \item \code{rmax} - Maximum relative humidity
#'     \item \code{rmin} - Minimum relative humidity
#'     \item \code{sph} - Near-surface specific humidity
#'     \item \code{srad} - Surface downwelling shortwave flux in air
#'     \item \code{th} - Wind direction at 10 m
#'     \item \code{tmmx} - Maximum air temperature
#'     \item \code{tmmn} - Minimum air temperature
#'     \item \code{vpd} - Mean vapor pressure deficit
#'     \item \code{vs} - Wind speed
#'   }
#' @param save_dir Character string specifying directory to save files.
#'   If NULL (default), uses current working directory.
#'
#' @return Invisibly returns a character vector of downloaded file paths.
#'   Prints download status messages.
#'
#' @examples
#' \dontrun{
#'   # Download TerraClimate precipitation data
#'   globalClimData(source = "TerraClimate",
#'                  years = 2020:2021,
#'                  variable = "ppt",
#'                  save_dir = "~/climate_data")
#'   
#'   # Download GridMET maximum temperature data
#'   globalClimData(source = "GridMET",
#'                  years = c(2018, 2019, 2020),
#'                  variable = "tmmx",
#'                  save_dir = "~/climate_data")
#'   
#'   # Download to current directory
#'   globalClimData(source = "TerraClimate",
#'                  years = 2022,
#'                  variable = "tmax")
#' }
#'
#' @seealso 
#' \code{\link{getTerraClimate.data}}, \code{\link{getGridMET.data}}
#'
#' @export
globalClimData <- function(source, years, variable, save_dir = NULL) {
  
  # Validate source parameter
  if (!is.character(source) || length(source) != 1) {
    stop("'source' must be a single character string", call. = FALSE)
  }
  
  # Normalize source to handle case variations
  source <- tolower(source)
  
  valid_sources <- c("terraclimate", "gridmet")
  
  if (!source %in% valid_sources) {
    stop("'source' must be either 'TerraClimate' or 'GridMET'", 
         call. = FALSE)
  }
  
  # Route to appropriate function
  message("Data source: ", toupper(substring(source, 1, 1)), 
          substring(source, 2))
  message(strrep("-", 50))
  
  result <- switch(
    source,
    "terraclimate" = getTerraClimate.data(
      years = years,
      variable = variable,
      save_dir = save_dir
    ),
    "gridmet" = getGridMET.data(
      years = years,
      variable = variable,
      save_dir = save_dir
    )
  )
  
  invisible(result)
}

