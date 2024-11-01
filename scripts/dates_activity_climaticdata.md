## Import distribution data per morph
```R
library(raster)
library(rgeos)
library(maps)
library(tidyverse)
library(CoordinateCleaner)
#where the data is
points_dir<-"/mnt/c/Users/facas/Dropbox/PhD_FabianSalgado/research_proposal/aim2/geographical_join_observations_morphs"
#load morph points
nonmelanic<-read_csv(paste0(points_dir,"/nonmelanic.csv"))
melanic<-read_csv(paste0(points_dir,"/melanic.csv"))
orange<-read_csv(paste0(points_dir,"/orange.csv"))
#join_data
total_data<-rbind(nonmelanic,melanic,orange)
total_data<-na.omit(total_data)

```
## Filter using Coordinatecleaner

```R
#nonmelanic
nomelanic<-clean_coordinates(x=nomelanic,lon = "X",lat = "Y",tests = c("seas","zeros","equal"))

#melanic
melanic<-clean_coordinates(x=melanic,lon = "X",lat = "Y",tests = c("seas","zeros","equal"))

#orange
orange<-clean_coordinates(x=orange,lon = "X",lat = "Y",tests = c("seas","zeros","equal"))
```

## Filter using spThin

```R
library(spThin)

# non melanic

nonmelanic_geoThin <- thin(loc.data = nonmelanic,
                         lat.col = "Y",
                         long.col = "X",
                         spec.col = "species",
                         thin.par = 5,
                         reps = 100,
                         locs.thinned.list.return = TRUE,
                         write.files = FALSE,
                         write.log.file = FALSE)

# melanic

melanic_geoThin <- thin(loc.data = melanic,
                         lat.col = "Y",
                         long.col = "X",
                         spec.col = "species",
                         thin.par = 5,
                         reps = 100,
                         locs.thinned.list.return = TRUE,
                         write.files = FALSE,
                         write.log.file = FALSE)

# orange

orange_geoThin <- thin(loc.data = orange,
                         lat.col = "Y",
                         long.col = "X",
                         spec.col = "species",
                         thin.par = 5,
                         reps = 100,
                         locs.thinned.list.return = TRUE,
                         write.files = FALSE,
                         write.log.file = FALSE)

#Select the data frame with the highest number of records

nonmelanic_geoThin <- as.data.frame(nonmelanic_geoThin[10]) %>% 
  mutate(morph = "nonmelanic")

melanic_geoThin <- as.data.frame(melanic_geoThin[100]) %>% 
  mutate(morph = "melanic")

orange_geoThin <- as.data.frame(orange_geoThin[33]) %>% 
  mutate(morph = "orange")


thin_nonmelanic<-NULL
for(i in 1:nrow(nonmelanic_geoThin)){
nonmelanic<-as.data.frame(nonmelanic)
rpos<-unique(intersect(grep(nonmelanic_geoThin[i,1], nonmelanic[,3]),grep(nonmelanic_geoThin[i,2], nonmelanic[,2])))
thin_nonmelanic<-rbind(thin_nonmelanic,nonmelanic[rpos,])
}

thin_melanic<-NULL
for(i in 1:nrow(melanic_geoThin)){
melanic<-as.data.frame(melanic)
rpos<-unique(intersect(grep(melanic_geoThin[i,1], melanic[,3]),grep(melanic_geoThin[i,2], melanic[,2])))
thin_melanic<-rbind(thin_melanic,melanic[rpos,])
}

thin_orange<-NULL
for(i in 1:nrow(orange_geoThin)){
orange<-as.data.frame(orange)
rpos<-unique(intersect(grep(orange_geoThin[i,1], orange[,3]),grep(orange_geoThin[i,2], orange[,2])))
thin_orange<-rbind(thin_orange,orange[rpos,])
}

write_csv(thin_nonmelanic,file="nonmelanic_geoThin_dates.csv")
write_csv(thin_melanic,file="melanic_geoThin_dates.csv")
write_csv(thin_orange,file="orange_geoThin_dates.csv")
#Load the points :)
thin_melanic<-read_csv("melanic_geoThin_dates.csv")
thin_nonmelanic<-read_csv("nonmelanic_geoThin_dates.csv")
thin_orange<-read_csv("orange_geoThin_dates.csv")


```


let's check the activity of the spider
```R
library(tidyverse)
total_obs<-rbind(thin_nonmelanic,thin_melanic,thin_orange)
total_obs$month<-str_split(string=total_obs$date,pattern="/") %>% lapply(FUN=function(x){return(x[2])}) %>% unlist() %>% gsub(pattern="^0",replacement="")
total_obs$month_names<-factor(month.name[as.numeric(total_obs$month)],levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
#proportion of register per motnh
prop.table(table(total_obs$month_names)) %>% sort()*100
#
ggplot(na.omit(total_obs),aes(x=month_names)) + geom_bar() + theme_classic()
```

```R
colour_pal<-c("#0D0887FF", "#3E049CFF", "#6300A7FF", "#F58C46FF", "#FDAD32FF", "#FCD225FF", "#F0F921FF", "#FCD225FF", "#FDAD32FF","#F58C46FF","#6300A7FF","#0D0887FF")
ggplot(na.omit(total_obs), aes(x = X, y = Y, color = factor(month_names))) +
  geom_point(size=4,alpha = 0.7) +
  #scale_color_discrete(name = "Month") +
  scale_color_manual(values = colour_pal) +
  labs(x = "Longitude", y = "Latitude")+ theme_classic()
  ```

Based on this let's discard the months with reports less than 5%

```R
month_reports<-prop.table(table(total_obs$month_names)) %>% sort()*100
months_activity<-which(month_reports>5) %>% names()
```

## get climatic data total observations

### Chelsa climatic layers

This data is available until 2019 with a resolution of 1km x 1km. we will use the script _download_chelsa.py_ to obtain the data for the months and variables we are interested in. Runnin the following line

```bash
python download_chelsea.py --years 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 --months 01 02 03 04 05 11 12 --variables pet cmi tasmin tasmax pr rsds --parallel --cores 10
```

After this to cut the layers to australia and calculate the mean of all the layers run the following

```R
# Load necessary libraries
library(terra)
library(stringr)

# Define the variables of interest
variables <- c("pet", "cmi", "tasmin", "tasmax", "pr", "rsds")

# Define a function to calculate the mean of CHIRPS data for Australia
mean_chelsa_australia <- function(variables) {

  # Load the reference polygon for Australia
  aus_pol <- terra::vect("/data/scratch/projects/punim1528/chelsea/aus_shape/gadm41_AUS_0.shp") #Dowloaded from https://gadm.org/

  # List all downloaded files
  files_downloaded <- list.files()

  # Iterate over each variable
  for (var in 1:length(variables)) {

    # Filter files related to the current variable
    files_var <- grep(pattern = variables[var], x = files_downloaded, value = TRUE)
    total_var_stack <- rast()
    year_stack <- rast()

    # Extract unique years from the files
    years <- unique(str_match(string = files_var, pattern = "\\d{4}"))[,1]

    # Iterate over each year
    for (year in years) {

      # Filter files for the current year
      files_year <- grep(pattern = year, x = files_var, value = TRUE)

      # Iterate over each file for the current year
      for(file in files_year) {
        raster_file <- rast(file)

        # Crop raster to Australia
        raster_file <- terra::crop(raster_file, aus_pol)

        # Add the raster to the year stack
        year_stack <- c(year_stack, raster_file)
      }

      # Calculate the mean of the year stack
      year_stack <- mean(year_stack)

      # Add the yearly mean to the total stack
      total_var_stack <- c(total_var_stack, year_stack)

      # Clean up memory
      gc()
    }

    # Calculate the mean of the total stack
    total_var_stack <- mean(total_var_stack)

    # Write the mean raster to a GeoTIFF file with a specific naming convention
    writeRaster(total_var_stack, filename = paste0(variables[var], ".tif"), overwrite = TRUE)
  }
}

# Call the function to calculate mean CHIRPS data for Australia for the specified variables
mean_chelsa_australia(variables)
```

After this you will have one layer per climatic variable. We will use this to extract information for all the dataset and frequency dataset


Let's start getting data from AWAP. for this we will import the function *awap_data_download*
```R
source("../scripts/awap_download_function.R")

```


### AWAP climatic data

To contrast with the previous data 

```R
total_obs$precipitation<-NA
total_obs$vapor_pressure<-NA
total_obs$max_temperature<-NA
total_obs$min_temperature<-NA
total_obs$solar_radiation<-NA
total_obs<-as.data.frame(total_obs)
for(obs in 1:nrow(total_obs)){
  output<-awap_data_download(datefinish_string = "01/01/2021",datestart_string = "01/01/2011",lat = total_obs[obs,"Y"],lon = total_obs[obs,"X"] ,months_activity = months_activity)
total_obs[obs,"precipitation"]<-output$precipitation
total_obs[obs,"vapor_pressure"]<-output$vapor_pressure
total_obs[obs,"max_temperature"]<-output$max_temperature
total_obs[obs,"min_temperature"]<-output$min_temperature
total_obs[obs,"solar_radiation"]<-output$solar_radiation
print(obs)
}
```


### CHELSA

Let's get the data from chelsa mean monthly layers

```R
# Load necessary libraries
library(terra)
library(tidyverse)
library(sf)

# Read all files with the ".tif" extension from the "chelsea" directory
chelsa_variables <- rast(list.files(path = "chelsea", pattern = ".tif", full.names = TRUE))

# Extract variable names from file names
names_variables <- list.files(path = "chelsea", pattern = ".tif$", full.names = TRUE) %>% 
                    str_match(pattern = "\\w+.tif$") %>% 
                    gsub(pattern = ".tif", replacement = "") %>% 
                    as.vector()

# Assign descriptive names to the raster layers
names(chelsa_variables) <- paste0("chelsa_", names_variables)

# Display the resolution of the raster layers
res(chelsa_variables)

# Create a data frame named 'total_obs' (assuming it's defined elsewhere)
total_obs <- data.frame(total_obs)

# Convert 'total_obs' to a spatial feature data frame (sf object) using coordinates X and Y
points <- st_as_sf(total_obs, coords = c("X", "Y"))

# Extract values from raster layers at the spatial locations defined by 'points'
chelsa_variables <- data.frame(terra::extract(chelsa_variables, points))

# Remove the first column from 'chelsa_variables' (assuming it's not needed)
chelsa_variables <- chelsa_variables[, -1]

# Combine 'total_obs' with 'chelsa_variables' by column-wise binding
total_obs <- cbind(total_obs, chelsa_variables)

```



## check colinearity between the predictors

### Chelsea variables

```R
library(tidyverse)  # For data manipulation and visualization
library(raster)     # For working with raster data
library(sp)         # For spatial data classes and methods



# Start PDF device for plotting
pdf("variables_similarity_chelsa_total.pdf")

#transform varibles for linear differences

total_obs$chelsa_pr<-log(total_obs$chelsa_pr)

# Calculate correlation matrix for selected variables and remove NAs
corr_matrix_total <- cor(na.omit(total_obs[, paste0("chelsa_", names_variables)]))

corrplot::corrplot(corr_matrix_total, method = "number", type = "upper", order = "original", tl.cex=1, )

# Convert correlation matrix to a distance matrix
predictors.dist <- as.dist(abs(corr_matrix_total))

# Cluster the variables based on their distances
predictors.cluster <- hclust(1 - predictors.dist)

# Plot the dendrogram of the cluster
plot(predictors.cluster)
abline(h = 0.5, lty = 2, col = "red")  # Add a horizontal line at distance 0.5

# End PDF device
dev.off()

# Check out the plot and select the more relevant variables based on distance cut point of 0.5



```

2. Correlation circle of a Principal Component Analysis.

Variables that are most parallel to each other are correlated; the closer (less angle between them) the more correlated.

```R
pca_totaldata<-ade4::dudi.pca(na.omit(total_obs[,paste0("chelsa_", names_variables)]),
                     scannf = F,
                     nf = 2)
pdf("pca_chelsa_total.pdf")
ade4::s.corcircle(pca_totaldata$co, grid = FALSE)
dev.off()
```

### AWAP variables

```R
library(tidyverse)  # For data manipulation and visualization
library(raster)     # For working with raster data
library(sp)         # For spatial data classes and methods



# Start PDF device for plotting
pdf("variables_similarity_awap_total.pdf")

#transform varibles for linear differences
total_obs$vapor_pressure<-log(total_obs$vapor_pressure)
# Calculate correlation matrix for selected variables and remove NAs
corr_matrix_total <- cor(na.omit(total_obs[, c("precipitation", "vapor_pressure", "max_temperature", "min_temperature", "solar_radiation")]))

corrplot::corrplot(corr_matrix_total, method = "number", type = "upper", order = "original", tl.cex=1, )


# Convert correlation matrix to a distance matrix
predictors.dist <- as.dist(abs(corr_matrix_total))

# Cluster the variables based on their distances
predictors.cluster <- hclust(1 - predictors.dist)

# Plot the dendrogram of the cluster
plot(predictors.cluster)
abline(h = 0.5, lty = 2, col = "red")  # Add a horizontal line at distance 0.5

# End PDF device
dev.off()

# Check out the plot and select the more relevant variables based on distance cut point of 0.5



```

2. Correlation circle of a Principal Component Analysis.

Variables that are most parallel to each other are correlated; the closer (less angle between them) the more correlated.

```R
pca_totaldata<-ade4::dudi.pca(na.omit(total_obs[,c("precipitation","vapor_pressure","max_temperature","min_temperature","solar_radiation")]),
                     scannf = F,
                     nf = 2)
pdf("pca_awap_total.pdf")
ade4::s.corcircle(pca_totaldata$co, grid = FALSE)
dev.off()
```

## export data space for further analyses

```R
save(list=ls(),file="input_niche_tmp_analyses.rda")
```

-----------------------------------------------------------------------------------------------------

## input for analyses field frequencies

### import data points

```R
library(sf)
library(tidyverse)
#remove variables
rm(list=ls())

points_dir<-"/mnt/c/Users/facas/Dropbox/PhD_FabianSalgado/research_proposal/aim2"
#load morph points
sites_freq<-read_csv(paste0(points_dir,"/colour_freq.csv"))
sites_freq$total<-apply(X=sites_freq[,grep(pattern="females",x=names(sites_freq))],1,sum)
sites_freq<-sites_freq[-which(sites_freq$total<80),]
points <- st_as_sf(sites_freq,coords = c("X","Y"),crs = 4326) %>% st_transform(3112)
```

### get climatic awap

Let's start getting data from AWAP. for this we will import the function *awap_data_download*
```R
source("../scripts/awap_download_function.R")

```

### AWAP climatic data

```R
sites_freq$precipitation<-NA
sites_freq$vapor_pressure<-NA
sites_freq$max_temperature<-NA
sites_freq$min_temperature<-NA
sites_freq$solar_radiation<-NA
sites_freq<-as.data.frame(sites_freq)
for(obs in 1:nrow(sites_freq)){
  output<-awap_data_download(datefinish_string = "01/01/2021",datestart_string = "01/01/2011",lat = sites_freq[obs,"Y"],lon = sites_freq[obs,"X"] ,months_activity = months_activity)
sites_freq[obs,"precipitation"]<-output$precipitation
sites_freq[obs,"vapor_pressure"]<-output$vapor_pressure
sites_freq[obs,"max_temperature"]<-output$max_temperature
sites_freq[obs,"min_temperature"]<-output$min_temperature
sites_freq[obs,"solar_radiation"]<-output$solar_radiation
print(obs)
}
```

### Chelsa climatic data

Let's get the data from chelsa mean monthly layers

```R
# Load necessary libraries
library(terra)
library(tidyverse)
library(sf)

# Read all files with the ".tif" extension from the "chelsea" directory
chelsa_variables <- rast(list.files(path = "chelsea", pattern = ".tif", full.names = TRUE))

# Extract variable names from file names
names_variables <- list.files(path = "chelsea", pattern = ".tif$", full.names = TRUE) %>% 
                    str_match(pattern = "\\w+.tif$") %>% 
                    gsub(pattern = ".tif", replacement = "") %>% 
                    as.vector()

# Assign descriptive names to the raster layers
names(chelsa_variables) <- paste0("chelsa_", names_variables)

# Display the resolution of the raster layers
res(chelsa_variables)

# Create a data frame named 'total_obs' (assuming it's defined elsewhere)
sites_freq <- data.frame(sites_freq)

# Convert 'total_obs' to a spatial feature data frame (sf object) using coordinates X and Y
points <- st_as_sf(sites_freq, coords = c("X", "Y"))

# Extract values from raster layers at the spatial locations defined by 'points'
chelsa_variables <- data.frame(terra::extract(chelsa_variables, points))

# Remove the first column from 'chelsa_variables' (assuming it's not needed)
chelsa_variables <- chelsa_variables[, -1]

# Combine 'total_obs' with 'chelsa_variables' by column-wise binding
sites_freq <- cbind(sites_freq, chelsa_variables)

```


## check colinearity between the predictors

### Chelsea variables

```R
library(tidyverse)  # For data manipulation and visualization
library(raster)     # For working with raster data
library(sp)         # For spatial data classes and methods



# Start PDF device for plotting
pdf("variables_similarity_chelsa_freq.pdf")

#linearized variables
sites_freq$chelsa_pr<-log(sites_freq$chelsa_pr)
sites_freq$chelsa_rsds<-log(sites_freq$chelsa_rsds)
# Calculate correlation matrix for selected variables and remove NAs
corr_matrix_total <- cor(na.omit(sites_freq[, paste0("chelsa_", names_variables)]))

corrplot::corrplot(corr_matrix_total, method = "number", type = "upper", order = "original", tl.cex=1, )

# Convert correlation matrix to a distance matrix
predictors.dist <- as.dist(abs(corr_matrix_total))

# Cluster the variables based on their distances
predictors.cluster <- hclust(1 - predictors.dist)

# Plot the dendrogram of the cluster
plot(predictors.cluster)
abline(h = 0.5, lty = 2, col = "red")  # Add a horizontal line at distance 0.5

# End PDF device
dev.off()

# Check out the plot and select the more relevant variables based on distance cut point of 0.5



```

2. Correlation circle of a Principal Component Analysis.

Variables that are most parallel to each other are correlated; the closer (less angle between them) the more correlated.

```R
pca_totaldata<-ade4::dudi.pca(na.omit(sites_freq[,paste0("chelsa_", names_variables)]),
                     scannf = F,
                     nf = 2)
pdf("pca_chelsa_freq.pdf")
ade4::s.corcircle(pca_totaldata$co, grid = FALSE)
dev.off()
```

### AWAP variables

```R
library(tidyverse)  # For data manipulation and visualization
library(raster)     # For working with raster data
library(sp)         # For spatial data classes and methods



# Start PDF device for plotting
pdf("variables_similarity_awap_freq.pdf")

#linearized models is not necessary


# Calculate correlation matrix for selected variables and remove NAs
corr_matrix_total <- cor(na.omit(sites_freq[, c("precipitation", "vapor_pressure", "max_temperature", "min_temperature", "solar_radiation")]))


corrplot::corrplot(corr_matrix_total, method = "number", type = "upper", order = "original", tl.cex=1, )


# Convert correlation matrix to a distance matrix
predictors.dist <- as.dist(abs(corr_matrix_total))

# Cluster the variables based on their distances
predictors.cluster <- hclust(1 - predictors.dist)

# Plot the dendrogram of the cluster
plot(predictors.cluster)
abline(h = 0.5, lty = 2, col = "red")  # Add a horizontal line at distance 0.5

# End PDF device
dev.off()

# Check out the plot and select the more relevant variables based on distance cut point of 0.5



```

2. Correlation circle of a Principal Component Analysis.

Variables that are most parallel to each other are correlated; the closer (less angle between them) the more correlated.

```R
pca_totaldata<-ade4::dudi.pca(na.omit(sites_freq[,c("precipitation","vapor_pressure","max_temperature","min_temperature","solar_radiation")]),
                     scannf = F,
                     nf = 2)
pdf("pca_awap_freq.pdf")
ade4::s.corcircle(pca_totaldata$co, grid = FALSE)
dev.off()
```

### calculate morph frequencies distances folowwing XXX

#### main function

```R
dist_morph_freq <- function(data, localities, morphs, method = "euclidean") {
  # Make sure the data is a data frame
  data <- as.data.frame(data)
  
  # Get unique locality values
  localities <- unique(localities)
  
  # Create an empty template with locality names as row names
  rownames(data) <- localities
  
  # Generate all possible locality comparisons
  comparisons <- expand.grid(localities, localities)
  
  # Generate an empty matrix for results
  result_guide <- matrix(NA, nrow = length(unique(comparisons$Var1)), ncol = length(unique(comparisons$Var1)))
  rownames(result_guide) <- colnames(result_guide) <- unique(comparisons$Var1)
  
  # Initialize comparisons$value with NA
  comparisons$value <- NA
  
  # Loop through each comparison
  for (cmp in 1:nrow(comparisons)) {
    # Get the localities to compare
    locA <- as.vector(comparisons[cmp, "Var1"])
    locB <- as.vector(comparisons[cmp, "Var2"])
    
    # If it is the same locality, move to the next comparison
    if (locA == locB) {
      comparisons[cmp, "value"] <- 0
      result_guide[locA, locB] <- 0
      next
    }
    
    # Initialize the value and morph counter
    value <- 0
    count_morphs <- 1
    
    # Calculate the dissimilarity value
    while (count_morphs < length(morphs)) {
      locA_total <- sum(data[locA, morphs])
      locB_total <- sum(data[locB, morphs])
      
      # Difference in frequency
      tmp_diff <- (data[locA, morphs[count_morphs]] / locA_total) - (data[locB, morphs[count_morphs]] / locB_total)
      
      # Apply the formula
      tmp_val <- tmp_diff^2
      
      # Add the value
      value <- sum(value, tmp_val)
      
      # Move to the next morph
      count_morphs <- count_morphs + 1
    }
    
    # When finished, add the final value to the dataframe and matrix
    comparisons[cmp, "value"] <- sqrt(value)
    result_guide[locA, locB] <- sqrt(value)
    result_guide[locB, locA] <- sqrt(value)
  }
  
  # Create a list containing the result matrix and comparisons dataframe
  result_list <- list(result_guide, comparisons)
  names(result_list) <- c("square_matrix", "pair_comp")
  
  # Return the result list
  return(result_list)
}

```

####

```R
result_dist_freq<-dist_morph_freq(data=sites_freq,localities=sites_freq$locality,morphs=c("females_white","females_orange","females_black"))
#geodistance
dis_geo<-geosphere::distm(as.matrix(sites_freq[,c("X","Y")]),fun=geosphere::distGeo)
```

### export data space for further analyses

```R
save(list=ls(),file="input_freq_analyses.rda")
```
