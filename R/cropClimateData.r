#' Simple Crop Climate Data to Region
#'
#' A straightforward function to crop NetCDF climate data to a specific region.
#'
#' @param file_path Path to the NetCDF file
#' @param country Country name (e.g., "United States of America", "Brazil")
#' @param state Optional state/province name or postal code (e.g., "MA", "California")
#' @param output_dir Directory to save the cropped file
#'
#' @return A SpatRaster object with the cropped data
#'
#' @export
cropClimateData <- function(file_path, country, state = NULL, output_dir = NULL) {
  
  # Load required libraries
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required. Install with: install.packages('terra')")
  }
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("Package 'rnaturalearth' is required. Install with: install.packages('rnaturalearth')")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with: install.packages('sf')")
  }
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  message("Step 1: Reading NetCDF file...")
  message("  File: ", basename(file_path))
  
  # Read the raster
  r <- terra::rast(file_path)
  message("  Dimensions: ", paste(dim(r), collapse = " x "))
  message("  CRS: ", terra::crs(r, describe = TRUE)$name)
  
  # Get boundary
  message("\nStep 2: Getting boundary...")
  
  if (is.null(state)) {
    # Get country boundary
    message("  Retrieving country: ", country)
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    
    # Try to find the country
    boundary <- world[tolower(world$name) == tolower(country) |
                      tolower(world$name_long) == tolower(country) |
                      toupper(world$iso_a3) == toupper(country) |
                      toupper(world$iso_a2) == toupper(country), ]
    
    if (nrow(boundary) == 0) {
      stop("Country '", country, "' not found. Available countries:\n",
           paste(head(world$name, 20), collapse = ", "), "...")
    }
    
    region_name <- boundary$name[1]
    
  } else {
    # Get state/province boundary
    message("  Retrieving state: ", state, " in ", country)
    
    # Standardize country name for US
    country_query <- country
    if (tolower(country) %in% c("usa", "us", "united states")) {
      country_query <- "United States of America"
    }
    
    states <- rnaturalearth::ne_states(country = country_query, returnclass = "sf")
    
    # Try to find the state
    if (nchar(state) <= 3) {
      # Assume it's a postal code
      boundary <- states[toupper(states$postal) == toupper(state), ]
    } else {
      # Assume it's a name
      boundary <- states[tolower(states$name) == tolower(state), ]
    }
    
    if (nrow(boundary) == 0) {
      stop("State '", state, "' not found. Available states:\n",
           paste(states$name, " (", states$postal, ")", collapse = ", ", sep = ""))
    }
    
    region_name <- boundary$name[1]
  }
  
  message("  Found: ", region_name)
  
  # Convert to terra vector
  message("\nStep 3: Converting boundary to terra format...")
  boundary_vect <- terra::vect(boundary)
  
  # Reproject if needed
  if (terra::crs(boundary_vect) != terra::crs(r)) {
    message("  Reprojecting boundary to match raster...")
    boundary_vect <- terra::project(boundary_vect, terra::crs(r))
  }
  
  # Crop
  message("\nStep 4: Cropping to bounding box...")
  r_crop <- terra::crop(r, boundary_vect)
  message("  New dimensions: ", paste(dim(r_crop), collapse = " x "))
  
  # Mask
  message("\nStep 5: Masking to exact boundary...")
  r_masked <- terra::mask(r_crop, boundary_vect)
  
  # Save if requested
  if (!is.null(output_dir)) {
    message("\nStep 6: Saving to file...")
    
    # Create directory if needed
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      message("  Created directory: ", output_dir)
    }
    
    # Construct output filename
    base_name <- tools::file_path_sans_ext(basename(file_path))
    if (!is.null(state)) {
      state_code <- if (nchar(state) <= 3) toupper(state) else toupper(boundary$postal[1])
      output_file <- file.path(output_dir, paste0(base_name, "_", state_code, ".nc"))
    } else {
      country_code <- boundary$iso_a3[1]
      output_file <- file.path(output_dir, paste0(base_name, "_", country_code, ".nc"))
    }
    
    message("  Output file: ", basename(output_file))
    
    terra::writeCDF(r_masked, 
                    filename = output_file,
                    overwrite = TRUE)
    message("  Saved successfully!")
  }
  
  message("\nDone! Returning cropped raster.")
  return(r_masked)
}
