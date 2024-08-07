# R Script to Calculate Tree Counts within 15m Buffers around UPRNs using Parallel Processing

# Load required libraries
library(sf)
library(dplyr)
library(foreach)
library(doParallel)

# Function to perform the main analysis
perform_analysis <- function(buffered_shp_path, points_shp_path, output_shp_path) {
  
  # Load shapefiles
  buffered <- st_read(buffered_shp_path)
  points <- st_read(points_shp_path)
  
  # Ensure CRS (Coordinate Reference System) match
  points <- st_transform(points, st_crs(buffered))
  
  # Create a unique identifier for each buffered area
  buffered$id <- 1:nrow(buffered)
  
  # Initialize parallel processing
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  # Perform calculations in parallel
  results_list <- foreach(i = 1:nrow(buffered), .combine = rbind) %dopar% {
    # Select the buffered area
    buffered_area <- buffered[i, ]
    
    # Create a 15m buffer around the buffered area
    buffered_area_15m <- st_buffer(buffered_area, dist = 15)
    
    # Spatial join to identify which points intersect with the buffered area
    points_in_buffered <- st_intersection(points, buffered_area_15m)
    
    # Calculate the count of intersecting points
    count <- nrow(points_in_buffered)
    
    # Return the result for this buffered area
    data.frame(Buffered_ID = buffered_area$id, Tree_Count = count)
  }
  
  # Stop parallel processing
  stopCluster(cl)
  
  # Convert results to a data frame
  results_df <- as.data.frame(results_list)
  
  # Merge results back with the original buffered areas
  buffered_results <- merge(buffered, results_df, by.x = "id", by.y = "Buffered_ID")
  
  # Save the result as a shapefile
  st_write(buffered_results, output_shp_path)
}


# Perform the analysis
perform_analysis(buffered_shp_path, points_shp_path, output_shp_path)
