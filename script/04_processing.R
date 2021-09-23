# PROCESSING LAYERS FOR SPECIES DISTRIBUTION MODELS #

library("data.table")
library("ggplot2")
library("grDevices")
library("patchwork")
library("raster")
library("sp")
library("sf")

# Files and parameters #

path <- "processing/enm"

# file with spp acronyms and uses #
spnames <- fread("data/species_names.csv")

# read the macs data

pass <- fread("data/macs.csv")

# shape with districts units #

adm <- st_read("MWI_adm/MWI_adm1.shp")

adm <- st_as_sf(adm)

adm <- adm[-c(0:0)]

adm

# use one of the bioclim layers and set all values as zero

f <- list.files("data/bioclim", 
                pattern = ".tif", 
                full.names = TRUE)[[2]]

mw <- raster(f)

mw[mw[] != 0] <- 0

myext <- extent(mw)

mw <- crop(mw, myext)

adm <- st_crop(adm, myext)

myproj <- as.character(crs(mw))

# ...............................................
# ...............................................
# Run over current presence ####

p_i <- paste0(path, "/MI/ensembles/presence/")

p_i <- stack(list.files(p_i,
                        pattern = "current.grd",
                        full.names = TRUE))

# presence is defined as 1, set all values different than 1 as NA
p_i[p_i[] != 1 ] <- NA

crs(p_i) <- myproj

p_i <- crop(p_i, myext)

# reconstruct layer using the regional layer as baseline
p_i <- mosaic(mw, p_i, fun = sum)

# set the regional layer as a mask
p_i <- mask(p_i, mw)

sp_p <- p_i

# Run over current distribution ####
# list with distribution layers

d_i <- paste0(path, "/MI/ensembles/suitability/")

d_i <- stack(list.files(d_i,
                        pattern = "current.grd",
                        full.names = TRUE))

crs(d_i) <- myproj

d_i <- crop(d_i, myext)

# reconstruct layer using the regional layer as baseline

d_i <- mosaic(mw, d_i, fun = sum)

# set the regional layer as a mask

d_i <- mask(d_i, mw)

# get the presence layers and set it as a mask for the distribution threshold #

p <- sp_p

p[p[] != 1] <- NA

d_i <- mask(d_i, p)

sp_d  <- d_i

# ............................................... #

# Create maps ####

#define colors for map of gradient of distribution

colpall <- colorRampPalette(c("#FFFF80", "#38E009","#1A93AB", "#0C1078"))

# Plot individual maps

output <- "output/individual_maps/"
dir.create(output, recursive = TRUE, showWarnings = FALSE)

# present map showing suitable areas for smallholder macadamia production in Malawi #

r <- sp_d
r <- as.data.frame(r, xy = TRUE)
r <- r[!is.na(r[, "layer"]), ]
r[, "layer"] <- round(r[, "layer"] / 1000, 2)

# lims for the colors

lims <- c(min(r$layer), max(r$layer))

p <- 
  ggplot() +
  geom_tile(r, mapping = aes(x = x, y = y, fill = layer)) +
  geom_sf(adm$geometry, mapping = aes(), colour = "black", fill = NA) +
  scale_fill_gradientn(name = NULL, colours = colpall(16),
                       breaks = lims,
                       limits = lims) +
  theme_void() + 
  labs(title = "Suitability of macadamia production in Malawi among smallholders") + 
  theme(legend.position = "right",
        plot.title = element_text(size = 14, 
                                  colour = "black")) 

plot(p)

ggsave(paste0(output, "macadamia_present.png"),
       plot = p,
       width = 20,
       height = 20,
       units = "cm")

# do the same for the future map and put the two together using patchwork