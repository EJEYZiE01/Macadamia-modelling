# CLIPPING WORLDCLIM DATA TO SPECIFIC STUDY SITE#

library("raster")
library("rgdal")
library("data.table")
library("raster")
library("dismo")
library("BiodiversityR")
library("car")
library("rgeos")
library("gbm")
library("glmnet")
library("maxnet")
library("sf")
library("PresenceAbsence")
library("devtools")
library("party")

gadm <- getData("GADM", country = "MWI", level = 0)

ext <- floor(extent(bbox(gadm)))

class(ext)

plot(gadm)

# Worldclim 2.1 #
ph <- "gadm/wc2.1_30s_bio" # Downloaded and stored in PC #

# Read rasters from an external hard drive
r <- stack(list.files(ph, 
                      pattern = ".tif",
                      full.names = TRUE))
# crop it
r <- crop(r, ext)

# mask it
r <- mask(r, gadm)

# check for a specific layer
plot(r[[1]])

# rename rasters
names(r)
names(r) <- gsub("wc2.1_30s_","",names(r))
names(r)

# save it on the project folder
output <- "data/bioclim/"

dir.create(output,
           showWarnings = FALSE,
           recursive = TRUE)

r <- stack(r)
names_r <- paste0(output, names(r))

list.files(output)
file.remove(list.files("data/bioclim/",
                       pattern = ".bil|.hdr|.tif", 
                       full.names = TRUE))
list.files(output)
writeRaster(r, 
            filename = names_r, 
            format = "GTiff",
            bylayer = TRUE,
            overwrite = TRUE)
plot(r)

# Elevation Raster #
# Vapor pressure #
# Solar radiation #
# Wind #

ph1 <- "gadm/wc2.1_30s_elev"
ph2 <- "gadm/wc2.1_30s_vapr"
ph3 <- "gadm/wc2.1_30s_srad"
ph4 <- "gadm/wc2.1_30s_wind"

# read rasters from an external hard drive
r1 <- stack(list.files(ph1, 
                       pattern = ".tif",
                       full.names = TRUE))

r2 <- stack(list.files(ph2, 
                       pattern = ".tif",
                       full.names = TRUE))

r3 <- stack(list.files(ph3, 
                       pattern = ".tif",
                       full.names = TRUE))

r4 <- stack(list.files(ph4, 
                       pattern = ".tif",
                       full.names = TRUE))

r <- stack(r1, r2, r3, r4)

# crop it
r <- crop(r, ext)

# mask it
r <- mask(r, gadm)

# check the first layer
plot(r)

# rename rasters
names(r)
names(r) <- gsub("wc2.1_30s_","",names(r))
names(r)

r <- stack(r)

names_r <- paste0(output, names(r))

list.files(output)

writeRaster(r, 
            filename = names_r, 
            format = "GTiff",
            bylayer = TRUE,
            overwrite = TRUE)