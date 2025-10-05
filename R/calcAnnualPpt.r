#' Calculate Total Annual Precipitation
#'
#' Simple function to calculate total annual precipitation from monthly 
#' TerraClimate precipitation data for a single year.
#'
#' @param ppt_file Path to precipitation NetCDF file (12 monthly layers)
#' @param output_file Optional path to save the result
#'
#' @return SpatRaster with total annual precipitation
#'
#' @examples
#' \dontrun{
#'   # Calculate for one year
#'   precip_1990 <- calcAnnualPpt(
#'     ppt_file = "TerraClimate_ppt_1990_MA.nc",
#'     output_file = "annual_precip_1990_MA.nc"
#'   )
#'   
#'   # Plot
#'   plot(precip_1990, main = "Total Annual Precipitation 1990")
#' }
#'
#' @export
calcAnnualPpt <- function(ppt_file, output_file = NULL) {
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required. Install with: install.packages('terra')")
  }
  
  # Read file
  message("Reading precipitation data: ", basename(ppt_file))
  ppt <- terra::rast(ppt_file)
  
  message("  Layers (months): ", terra::nlyr(ppt))
  
  # Calculate total annual precipitation (sum of 12 months)
  message("Calculating total annual precipitation...")
  annual_total <- sum(ppt)
  
  # Set name
  year <- gsub(".*_(\\d{4}).*", "\\1", basename(ppt_file))
  names(annual_total) <- paste0("total_precip_", year)
  
  # Save if requested
  if (!is.null(output_file)) {
    message("Saving to: ", basename(output_file))
    
    # Create directory if needed
    out_dir <- dirname(output_file)
    if (!dir.exists(out_dir) && out_dir != ".") {
      dir.create(out_dir, recursive = TRUE)
    }
    
    terra::writeCDF(annual_total, 
                    filename = output_file,
                    overwrite = TRUE)
  }
  
  message("Done!")
  return(annual_total)
}
