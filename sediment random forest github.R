library(terra)
library(randomForest)
setwd("...") #best to have all input layers in a single location, and have that location as the working directory

#load sediment grab points into R. Replace ... with the fie name of your CSV. 
#names(sed) <- c() gives the headers of each of the columns in the CSV. These should be in the same order and written the same way they appear in the CSV.
points <- read.csv("...")

#remove missing values. Mud, sand, and gravel values have been divided by 100 so that all values are now between 0 and 1
sed <- points[complete.cases(points), ]
sed$mud <- sed$mud/100
sed$sand <- sed$sand/100
sed$gravel <- sed$gravel/100
sed <- sed[complete.cases(sed),]

#names(sed) <- c() gives the headers of each of the columns in the CSV. These should be in the same order and written the same way they appear in the CSV.
names(sed) <- c("gravel","sand","mud","current_direction","current_magnitude","ED_islands","ED_mainland","bathymetry","maxwavepow",
                "ustarmean","ustarmax","meanwavepow","eastness","northness","ruggedness","slope","bbpi","fbpi","bathy_mean","bathy_stdev","bathy_var")

#load in the predictor rasters, using one of them as a mask so that they're all aligned. I used bathymetry as my mask, which is what 'bath' is
#replace ... with the name of the raster you're using.
bath <- rast("...")
#function for applying the mask to the other rasters
resample_and_mask <- function(raster_file, mask_raster) {
  raster_data <- rast(raster_file)
  resampled_raster <- resample(raster_data, mask_raster)
  masked_raster <- mask(resampled_raster, mask_raster)
  return(masked_raster)
}

bbpi_masked <- resample_and_mask("...", bath)
east_masked <- resample_and_mask("...", bath)
fbpi_masked <- resample_and_mask("...", bath)
north_masked <- resample_and_mask("...", bath)
curd_masked <- resample_and_mask("...", bath)
curm_masked <- resample_and_mask("...", bath)
rug_masked <- resample_and_mask("...", bath)
slope_masked <- resample_and_mask("...", bath)
umean_masked <- resample_and_mask("...", bath)
umax_masked <- resample_and_mask("...", bath)
wmax_masked <- resample_and_mask("...", bath)
wmean_masked <- resample_and_mask("...", bath)
edm_masked <- resample_and_mask("...", bath)
edi_masked <- resample_and_mask("...", bath)
mean_masked <- resample_and_mask("...", bath)
stdev_masked <- resample_and_mask("...", bath)
var_masked <- resample_and_mask("...", bath)

#create a raster stack to put into the random forest model
rfstack <- c(bath, bbpi_masked, east_masked, fbpi_masked, north_masked, curd_masked,
             curm_masked, rug_masked, slope_masked, umean_masked, umax_masked,
             wmax_masked, wmean_masked, edm_masked, edi_masked, mean_masked,
             stdev_masked, var_masked)

#names should match the names in the CSV headers
names(rfstack) <- c("bathymetry","bbpi","eastness","fbpi","northness","current_direction","current_magnitude","ruggedness","slope","ustarmean",
                    "ustarmax","maxwavepow","meanwavepow","ED_mainland","ED_islands","bathy_mean","bathy_stdev","bathy_var")
#model one random forest for each of mud, sand, and gravel. Use ntree= to adjust number of trees and mtry= to adjust number of variables at each node
rfm <- randomForest(mud ~  current_direction  + current_magnitude  + ED_islands  + ED_mainland  + bathymetry  + maxwavepow  +
                      ustarmean  + ustarmax  + meanwavepow  + eastness  + northness  + ruggedness  + slope  + bbpi  + fbpi  + bathy_mean + bathy_var,  sed)
rfs <- randomForest(sand ~ current_direction  + current_magnitude  + ED_islands  + ED_mainland  + bathymetry  + maxwavepow  +
                      ustarmean  + ustarmax  + meanwavepow  + eastness  + northness  + ruggedness  + slope  + bbpi  + fbpi  + bathy_mean + bathy_var,  sed)
rfg <- randomForest(gravel ~ current_direction  + current_magnitude  + ED_islands  + ED_mainland  + bathymetry  + maxwavepow  +
                      ustarmean  + ustarmax  + meanwavepow  + eastness  + northness  + ruggedness  + slope  + bbpi  + fbpi  + bathy_mean + bathy_var,  sed)
gravel <- sed$gravel
sand <- sed$sand
mud <- sed$mud

#apply random forest models to the study area
mpred <- terra::predict(rfstack,model=rfm)
spred <- terra::predict(rfstack,model=rfs)
gpred <- terra::predict(rfstack,model=rfg)

#use writeRaster() to generate rasters of your sediment predictions. Replace ... with the desired filename, including the file path.
writeRaster(mpred,filename="...", filetype="GTiff", overwrite=TRUE)
writeRaster(spred,filename="...", filetype="GTiff", overwrite=TRUE)
writeRaster(gpred,filename="...", filetype="GTiff", overwrite=TRUE)
