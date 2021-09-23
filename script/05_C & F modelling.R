# SCRIPT TO PROCESS LAYERS FROM ENSEMBLE MODELLING #


# Define extension of study region #
ext <- raster::extent(30, 40,  -18, -9)
proj <- "+proj=longlat +datum=WGS84" 

# Read species names and acronyms #
sp <- fread("data/species_acronyms.csv")
sp %<>% 
  filter(use != "Crop") %>%
  dplyr::select(acronym, use)

# names of RCP scenarios and crops
crop <- c("MI")
RCP <- c(45, 85)

# Read layer of inland water and remove areas of raster in water (lakes, rivers, etc) #
#source http://www.diva-gis.org/Data #
lakes <- "MWI_wat/MWI_water_areas_dcw.shp"
lakes %<>%  
  readOGR(.) %>%
  subset(.$HYC_DESCRI == "Perennial/Permanent") %>%
  raster::crop(. , ext)

# Read country borders #
border <- "MWI_adm/MWI_adm1.shp"
border %<>% 
  readOGR(.) %>%
  raster::crop(. , ext)

# Read rasters of macadamia species #
# Run over the raster of current presence-absent #
macc <- list.files(paste0("processing/enm/MI/ensembles/presence"),
                pattern = "current.gri$", 
                full.names = TRUE)
  
macc %<>% 
  raster::stack( . ) %>% 
  raster::crop(. , ext) %>% 
  raster::mask(. , border, inverse = FALSE) %>% 
  raster::mask(. , lakes, inverse = TRUE) %>% 
  raster::stack()

macc

names(macc) <- gsub("_current_presence", "", names(macc))

output <- "processing/layers/"
dir.create(output,
           showWarnings = FALSE,
           recursive = TRUE)

# Export all layers combined as a single raster
writeRaster(macc, 
            filename = "./processing/layers/current.grd", 
            format = "raster", 
            overwrite = TRUE, 
            bylayer = FALSE)

# Calculate total area for current macadamia suitability per district #

dis.suit <- raster::extract(macc, border, fun=sum, na.rm=TRUE,df=TRUE)
dis.suit.name <- cbind(dis.suit, border@data)
head(dis.suit.name)
dis.suit.name$prop <- dis.suit.name$MI_current_presence*20/dis.suit.name$Area*100

# Read layers of future presence for all species #
macc_rcp <- list()

for (j in seq_along(RCP)){
  
  #read rasters of j rcp scenarios
  r <- list.files(paste0("processing/enm/MI/ensembles/presence"),
                  pattern = paste0(RCP[j],".gri$"),
                  full.names = TRUE)
  r %<>%
    raster::stack(. ) %>%
    raster::crop(. , ext) %>% #crop rasters within defined extension
    raster::calc(. , fun = mean) %>% #calculate the mean of all k rcp scenarios
    raster::mask(. , border, inverse = FALSE) %>% 
    raster::mask(. , lakes, inverse = TRUE)
  
  # if less than 66% of models agree with the presence then 0
  r[r[] < 0.659 ] <- 0
  # if more than 66% of models agree with the presence then 1 
  r[r[] > 0.659 ] <- 1 
  rname <- paste0(sp$acronym[i], RCP[j])
  names(r) <- rname
  macc_rcp[[rname]] <- r
  
} 

# Future suitability
# run over crop names and rcp scenarios to identify changes in suitability between scenarios #

macc_change <- list()
  
for (j in seq_along(macc_rcp)){

  rf <- macc_rcp[[j]]
  #reclassify raster, values 
  rf[rf[] > 0] <- 2 # if presence then assign 2 

  #identify changes in suitability between current and j rcp scenario 
  #change in suitability codes
  #  1 = always suitable
  #  0 = never suitable
  # -1 = no longer suitable
  #  2 = new habitat
  rf %<>%
    raster::overlay( . , macc, fun = function(x,y) {(x-y)})
  
  writeRaster(rf, 
              filename = paste0("processing/layers/MI_rcp", RCP[j],"_change1.img"), 
              format="HFA", 
              overwrite=TRUE)
  name_rf <- paste0(crop[i],RCP[j],"_change",sep="")
  
  names(rf) <- name_rf
  
  macc_change[[  name_rf ]] <- rf
  
}

# separate rasters of change in suitability for each code
# calculate area lost in km2

?raster::area

changes <- tibble(code = c(-1, 0, 1, 2),
                  change = c("no_longer", "never", "suitable","new_habitat"), 
                  rpc45 = tapply(area(macc_change[[1]]), macc_change[[1]][], sum),
                  rcp85 = tapply(area(macc_change[[2]]), macc_change[[2]][], sum))
changes


