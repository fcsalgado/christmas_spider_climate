library(terra)
library(stringr)

variables <- c("pet","cmi","tasmin","tasmax","pr","rsds")

mean_chelsa_australia <- function(variables) {

#reference polygon Australia

aus_pol<-terra::vect("/data/scratch/projects/punim1528/chelsea/aus_shape/gadm41_AUS_0.shp")

  
  files_downloaded <- list.files()
   
  for (var in 1:length(variables)) {
    
    files_var <- grep(pattern = variables[var], x = files_downloaded, value = TRUE)
    total_var_stack <- rast()
    year_stack <- rast()

    years<-unique(str_match(string=files_var,pattern="\\d{4}"))[,1]

    for (year in years) {
        
        # Read the raster file into a raster stack
        files_year <- grep(pattern = year, x = files_var, value = TRUE)

        for(file in files_year){ 
        raster_file <- rast(file)

        raster_file<-terra::crop(raster_file,aus_pol)
        
        # Calculate the average value across all cells in the raster stack
              
       # Add the average value raster layer to the raster stack
        year_stack <- c(year_stack, raster_file)
      }
    
    year_stack <- mean(year_stack)

    total_var_stack<-c(total_var_stack,year_stack) 

    #rm(year_stack)

    gc()
    # Write the mean raster to a GeoTIFF file with a specific naming convention
    
  }

total_var_stack <- mean(total_var_stack)

writeRaster(total_var_stack, filename = paste0(variables[var], ".tif"), overwrite = TRUE)
  }  
}

# Call the function
mean_chelsa_australia(variables)




