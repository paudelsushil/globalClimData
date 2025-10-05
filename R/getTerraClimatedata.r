#' Download TerraClimate Data
#'
#' Downloads climate variable data files from the Northwest Knowledge Network
#' TerraClimate dataset for specified years and variables.
#'
#' @param years Numeric vector of years to download (must be >= 1958)
#' @param variable Character vector specifying the climate variable(s) to download.
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
#'   # Download multiple variables for multiple years
#'   getTerraClimate.data(years = c(2018, 2019, 2020), 
#'                        variable = c("tmax", "tmin", "ppt"))
#'   
#'   # Download all drought-related variables
#'   getTerraClimate.data(years = 2020, 
#'                        variable = c("def", "PDSI", "soil", "swe"))
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
  
  if (!is.character(variable)) {
    stop("'variable' must be a character vector", call. = FALSE)
  }
  
  # Valid TerraClimate variables
  valid_vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad", 
                  "swe", "tmax", "tmin", "vap", "ws", "vpd", "PDSI")
  
  # Check all variables are valid
  invalid_vars <- setdiff(variable, valid_vars)
  if (length(invalid_vars) > 0) {
    stop("Invalid variable(s): ", paste(invalid_vars, collapse = ", "), 
         "\nValid options: ", paste(valid_vars, collapse = ", "), 
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
  
  # Create all combinations of years and variables
  download_grid <- expand.grid(
    year = years, 
    variable = variable,
    stringsAsFactors = FALSE
  )
  
  total_files <- nrow(download_grid)
  downloaded_files <- character(total_files)
  
  # Print download plan
  message("\n", strrep("=", 60))
  message("Download Plan:")
  message("  Years: ", paste(range(years), collapse = " to "), 
          " (", length(years), " year", ifelse(length(years) > 1, "s", ""), ")")
  message("  Variables: ", paste(variable, collapse = ", "), 
          " (", length(variable), " variable", ifelse(length(variable) > 1, "s", ""), ")")
  message("  Total files: ", total_files)
  message("  Destination: ", save_dir)
  message(strrep("=", 60), "\n")
  
  # Download files for each combination
  for (i in seq_len(total_files)) {
    year <- download_grid$year[i]
    var <- download_grid$variable[i]
    file_name <- paste0("TerraClimate_", var, "_", year, ".nc")
    file_url <- paste0(base_url, file_name)
    dest_path <- file.path(save_dir, file_name)
    
    message("[", i, "/", total_files, "] Downloading: ", file_name)
    
    # Download with error handling
    tryCatch({
      utils::download.file(
        url = file_url,
        destfile = dest_path,
        mode = "wb",
        quiet = FALSE
      )
      downloaded_files[i] <- dest_path
      message("  -> Success")
    }, error = function(e) {
      warning("Failed to download ", file_name, ": ", e$message, 
              call. = FALSE, immediate. = TRUE)
      downloaded_files[i] <- NA_character_
      message("  -> Failed")
    })
  }
  
  # Summary message
  n_success <- sum(!is.na(downloaded_files))
  n_failed <- sum(is.na(downloaded_files))
  
  message("\n", strrep("=", 60))
  message("Download Summary:")
  message("  Successfully downloaded: ", n_success, " file(s)")
  if (n_failed > 0) {
    message("  Failed downloads: ", n_failed, " file(s)")
    failed_indices <- which(is.na(downloaded_files))
    message("\n  Failed files:")
    for (idx in failed_indices) {
      message("    - TerraClimate_", download_grid$variable[idx], "_", 
              download_grid$year[idx], ".nc")
    }
  }
  message("  Location: ", save_dir)
  message(strrep("=", 60))
  
  # Return paths invisibly
  invisible(downloaded_files[!is.na(downloaded_files)])
}