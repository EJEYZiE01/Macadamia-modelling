# CLIPPING WORLDCLIM DATA TO SPECIFIC STUDY SITE ADAPTED FROM KAUEDESOUSA #

# source data is available in an external hard drive or 
# http://worldclim.org/version2.1
library("raster")
library("rgdal")

gadm <- getData("GADM", country = "MWI", level = 0)

ext <- floor(extent(bbox(gadm)))

r <- raster(ext, res = 0.04166666)

gadm <- rasterize(gadm, r, field = 1)

plot(gadm)

# ...........................................
# ...........................................
# worldclim 2.0

# read rasters from an external hard drive
r <- stack(list.files("/Volumes/BioversityInt/rasters/worldclim1_0/current/", 
                      pattern = ".bil$",
                      full.names = TRUE))
# crop it
r <- crop(r, ext)

# mask it
r <- mask(r, gadm)

# check the first layer
plot(r[[1]])

# rename rasters
names(r)
names(r) <- gsub("wc2.1_2.5m_","",names(r))
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

# ..................................
# ..................................
# future scenarios
gcm <- list.dirs("/Volumes/BioversityInt/rasters/worldclim1_0/gcm")[-1]

# create a local folder
output <- "data/gcm/"

dir.create(output,
           showWarnings = FALSE,
           recursive = TRUE)

for (i in seq_along(gcm)) {
  print(gcm[[i]])
  # read rasters from an external hard drive
  r <- stack(list.files(gcm[[i]], 
                        pattern = ".tif$",
                        full.names = TRUE))
  
  # crop it
  r <- crop(r, ext)
  
  # mask it
  r <- mask(r, gadm)
  
  #model <- substr(names(r), 1, 2)[1]
  #bio <- gsub(paste0(model,"45bi7"),"", names(r))
  #bio[nchar(bio) > 2] <- gsub("01","1", bio[nchar(bio) > 2])
  #bio <- paste0("bio_", bio)
  #names(r) <- bio
  
  model <- strsplit(gcm[[i]], "/")
  model <- model[[1]][length(model[[1]])]
  
  output_i <- paste0(output, model, "/")
  dir.create(output_i, recursive = TRUE, showWarnings = FALSE)
  
  names_r <- paste0(output_i, names(r))
  
  file.remove(list.files(output_i, full.names = TRUE))
  
  writeRaster(r, 
              filename = names_r, 
              format = "GTiff",
              bylayer = TRUE,
              overwrite = TRUE)
  
  
}
