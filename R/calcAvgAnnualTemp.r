#' Calculate Annual Average Temperature
#'
#' Simple function to calculate annual average temperature from monthly 
#' tmax and tmin TerraClimate data for a single year.
#'
#' @param tmax_file Path to tmax NetCDF file (12 monthly layers)
#' @param tmin_file Path to tmin NetCDF file (12 monthly layers)
#' @param output_file Optional path to save the result
#'
#' @return SpatRaster with annual average temperature
#'
#' @examples
#' \dontrun{
#'   # Calculate for one year
#'   temp_1990 <- calcAnnualAvgTemp(
#'     tmax_file = "TerraClimate_tmax_1990_MA.nc",
#'     tmin_file = "TerraClimate_tmin_1990_MA.nc",
#'     output_file = "avg_temp_1990_MA.nc"
#'   )
#'   
#'   # Plot
#'   plot(temp_1990)
#' }
#'
#' @export
calcAnnualAvgTemp <- function(tmax_file, tmin_file, output_file = NULL) {
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required. Install with: install.packages('terra')")
  }
  
  # Read files
  message("Reading tmax: ", basename(tmax_file))
  tmax <- terra::rast(tmax_file)
  
  message("Reading tmin: ", basename(tmin_file))
  tmin <- terra::rast(tmin_file)
  
  # Check they have same number of layers
  if (terra::nlyr(tmax) != terra::nlyr(tmin)) {
    stop("tmax and tmin must have same number of layers")
  }
  
  # Calculate mean temp for each month: (tmax + tmin) / 2
  message("Calculating monthly mean temperatures...")
  monthly_mean <- (tmax + tmin) / 2
  
  # Calculate annual average across all 12 months
  message("Calculating annual average...")
  annual_avg <- terra::mean(monthly_mean)
  
  # Set name
  year <- gsub(".*_(\\d{4}).*", "\\1", basename(tmax_file))
  names(annual_avg) <- paste0("avg_temp_", year)
  
  # Save if requested
  if (!is.null(output_file)) {
    message("Saving to: ", basename(output_file))
    
    # Create directory if needed
    out_dir <- dirname(output_file)
    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE)
    }
    
    terra::writeCDF(annual_avg, 
                    filename = output_file,
                    overwrite = TRUE)
  }
  
  message("Done!")
  return(annual_avg)
}
