#' Download TerraClimate Data
#'
#' Downloads climate variable data files from the Northwest Knowledge Network
#' TerraClimate dataset for specified years.
#'
#' @param years Numeric vector of years to download (must be >= 1958)
#' @param variable Character string specifying the climate variable to download.
#'   Available variables:
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
#' @param save_dir Character string specifying directory to save files.
#'   If NULL (default), uses current working directory.
#'
#' @return Invisibly returns a character vector of downloaded file paths.
#'   Prints download status messages.
#'
#' @examples
#' \dontrun{
#'   # Download precipitation data for 2020-2021
#'   getTerraClimate.data(years = 2020:2021, 
#'                        variable = "ppt", 
#'                        save_dir = "~/climate_data")
#'   
#'   # Download maximum temperature for multiple years
#'   getTerraClimate.data(years = c(2018, 2019, 2020), 
#'                        variable = "tmax")
#' }
#'
#' @export
getTerraClimate.data <- function(years, variable, save_dir = NULL) {
  
  # Validate inputs
  if (!is.numeric(years)) {
    stop("'years' must be a numeric vector", call. = FALSE)
  }
  
  if (any(years < 1958)) {
    stop("All years must be >= 1958 (TerraClimate data starts in 1958)", 
         call. = FALSE)
  }
  
  if (any(years > as.numeric(format(Sys.Date(), "%Y")))) {
    stop("Years cannot be in the future", call. = FALSE)
  }
  
  if (!is.character(variable) || length(variable) != 1) {
    stop("'variable' must be a single character string", call. = FALSE)
  }
  
  # Valid TerraClimate variables
  valid_vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad", 
                  "swe", "tmax", "tmin", "vap", "ws", "vpd", "PDSI")
  
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
  
  base_url <- "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/"
  
  # Track downloaded files
  downloaded_files <- character(length(years))
  
  # Download files for each year
  for (i in seq_along(years)) {
    year <- years[i]
    file_name <- paste0("TerraClimate_", variable, "_", year, ".nc")
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