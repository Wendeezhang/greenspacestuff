# Load required libraries
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(r5r)
library(nngeo)  # For nearest neighbor queries
library(fs)
library(rJava)

# Initialize Java for rJava package
install.packages("rJava")
.jinit()
options(java.parameters = '-Xmx2G')

# Define file paths
origins_path <- "~/Documents/filtered_shapefile_with_coordinates.shp"
destinations_path <- "~/Desktop/DATA/Export_Output_5.shp"
green_spaces_path <- "~/Desktop/DATA/green_spaces.shp"  # Replace with actual green spaces shapefile path
results_path <- "~/results.csv"

# Load the shapefiles
origins <- st_read(origins_path)
destinations <- st_read(destinations_path, quiet = TRUE)
destinations$id <- seq_len(nrow(destinations))

# Inspect the shapefile to find the correct column name for residential codes
print(colnames(origins))  # Print column names to identify the correct one

# Assume 'residential_code_column' is the correct column name after inspection
residential_code_column <- 'your_column_name_here'  # Replace with actual column name

# Define residential codes
codes <- c(
  "R", "RB", "RC", "RC01", "RD", "RD01", "RD02", "RD03", "RD04",
  "RD06", "RD07", "RD08", "RD10", "RG", "RG02", "RH", "RH01",
  "RH02", "RH03", "RI", "RI01", "RI02", "RI02NC", "RI02RC", "RI03"
)

# Filter origins based on residential codes
filtered_origins <- origins %>%
  filter(!!sym(residential_code_column) %in% codes)

# Add coordinates to the filtered origins
coordinates <- st_coordinates(filtered_origins)
filtered_origins$X <- coordinates[, 1]
filtered_origins$Y <- coordinates[, 2]

# Save the filtered shapefile
st_write(filtered_origins, "~/filtered_origins.shp")

# Load the filtered shapefile with coordinates
filtered_origins <- st_read("~/filtered_origins.shp")

# Load green space data
green_spaces <- st_read(green_spaces_path)

# Ensure both filtered_origins and green_spaces are in the same CRS
filtered_origins <- st_transform(filtered_origins, st_crs(green_spaces))

# Perform nearest neighbor search
nn_results <- nngeo::st_nn(filtered_origins, green_spaces, k = 3, returnDist = TRUE)

# Initialize data frame to collect results
results_df <- data.frame()

# Reproject destinations to the same CRS as filtered_origins
destinations <- st_transform(destinations, crs = st_crs(filtered_origins))

# Set up r5r
r5r_core <- setup_r5(data_path = "~/Desktop/DATA", verbose = FALSE)

# Define parameters for travel time matrix calculation
mode <- "WALK"  # Mode of transport
max_walk_dist <- 5000  # Max walking distance (meters)
max_trip_duration <- 60  # Max trip duration (minutes)
departure_datetime <- as.POSIXct("2023-09-21 14:00:00")

# Iterate through each origin (UPRN)
for (i in 1:nrow(filtered_origins)) {
  tryCatch({
    origin_row <- filtered_origins[i, ]
    green_space_indices <- nn_results$nn[[i]]
    green_space_distances <- nn_results$distances[[i]]
    nearest_green_spaces <- green_spaces[green_space_indices, ]
    
    # Calculate travel time matrix for the current origin and its nearest green spaces
    ttm <- travel_time_matrix(
      r5r_core = r5r_core,
      origins = origin_row,
      destinations = nearest_green_spaces,
      mode = mode,
      departure_datetime = departure_datetime,
      max_trip_duration = max_trip_duration,
      verbose = FALSE
    )
    
    # Combine results with the corresponding distances
    ttm_results <- data.frame(
      origin_id = origin_row$id,
      destination_id = nearest_green_spaces$id,
      travel_time = ttm$travel_time_p50
    )
    
    # Add distances for comparison
    ttm_results$distance <- green_space_distances
    
    # Select the nearest destination with the minimum travel time
    closest_result <- ttm_results %>%
      arrange(travel_time) %>%
      slice(1)
    
    # Append the result row to the results data frame
    results_df <- rbind(results_df, closest_result)
    
  }, error = function(e) {
    cat("Error occurred for origin", i, ":", conditionMessage(e), "\n")
  })
}

# Save the results as a CSV file
write.csv(results_df, file = results_path, row.names = FALSE)

# Load necessary libraries for heatmap
library(dplyr)
library(ggplot2)
library(data.table)

# Load the TTM results and filtered shapefile
ttm_data <- fread(results_path)
filtered_origins <- st_read("~/filtered_origins.shp", quiet = TRUE)
filtered_origins <- filtered_origins %>%
  mutate(ID = row_number())

# Merge TTM data with coordinates
heatmap_data <- merge(filtered_origins, ttm_data, by.x = "ID", by.y = "origin_id")

# Create a heatmap using ggplot2
ggplot(heatmap_data, aes(x = X, y = Y, fill = travel_time)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Travel Time Heatmap", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Print the heatmap data
print(heatmap_data)

