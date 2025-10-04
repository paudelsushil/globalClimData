#' Download GridMET Climate Data
#'
#' Downloads climate variable data files from the Northwest Knowledge Network
#' GridMET dataset for specified years.
#'
#' @param years Numeric vector of years to download (must be >= 1979)
#' @param variable Character string specifying the climate variable to download.
#'   Available variables:
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
#'   # Download precipitation data for 2020-2021
#'   getGridMET.data(years = 2020:2021, 
#'                   variable = "pr", 
#'                   save_dir = "~/climate_data")
#'   
#'   # Download maximum temperature for multiple years
#'   getGridMET.data(years = c(2018, 2019, 2020), 
#'                   variable = "tmmx")
#' }
#'
#' @export
getGridMET.data <- function(years, variable, save_dir = NULL) {
  
  # Validate inputs
  if (!is.numeric(years)) {
    stop("'years' must be a numeric vector", call. = FALSE)
  }
  
  if (any(years < 1979)) {
    stop("All years must be >= 1979 (GridMET data starts in 1979)", 
         call. = FALSE)
  }
  
  if (any(years > as.numeric(format(Sys.Date(), "%Y")))) {
    stop("Years cannot be in the future", call. = FALSE)
  }
  
  if (!is.character(variable) || length(variable) != 1) {
    stop("'variable' must be a single character string", call. = FALSE)
  }
  
  # Valid GridMET variables
  valid_vars <- c("bi", "erc", "etr", "pet", "pr", "rmax", "rmin", 
                  "sph", "srad", "th", "tmmx", "tmmn", "vpd", "vs")
  
  if (!variable %in% valid_vars) {
    stop("'variable' must be one of: ", paste(valid_vars, collapse = ", "), 
         call. = FALSE)
  }
  
  # Set up save directory
  if (is.null(save_dir)) {
    save_dir <- getwd()
  } else {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
      message("Created directory: ", save_dir)
    }
  }
  
  base_url <- "https://www.northwestknowledge.net/metdata/data/"
  
  # Track downloaded files
  downloaded_files <- character(length(years))
  
  # Download files for each year
  for (i in seq_along(years)) {
    year <- years[i]
    file_name <- paste0(variable, "_", year, ".nc")
    file_url <- paste0(base_url, file_name)
    dest_path <- file.path(save_dir, file_name)
    
    message("Downloading: ", file_name, " (", i, "/", length(years), ")")
    
    # Download with error handling
    tryCatch({
      utils::download.file(
        url = file_url,
        destfile = dest_path,
        mode = "wb",
        quiet = FALSE
      )
      downloaded_files[i] <- dest_path
      message("  -> Saved to: ", dest_path)
    }, error = function(e) {
      warning("Failed to download ", file_name, ": ", e$message, call. = FALSE)
      downloaded_files[i] <- NA_character_
    })
  }
  
  # Summary message
  n_success <- sum(!is.na(downloaded_files))
  n_failed <- sum(is.na(downloaded_files))
  
  message("\n", strrep("=", 50))
  message("Download Summary:")
  message("  Successfully downloaded: ", n_success, " file(s)")
  if (n_failed > 0) {
    message("  Failed downloads: ", n_failed, " file(s)")
  }
  message("  Location: ", save_dir)
  message(strrep("=", 50))
  
  # Return paths invisibly
  invisible(downloaded_files[!is.na(downloaded_files)])
}