# MODELLING SPECIES DISTRIBUTION USING BIOCLIMATIC VARIABLES # 

library("raster")
library("rgdal")
library("data.table")
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

# Data the BiodiversityR saves outputs on the current working directory
# get the parent wd to return here if needed #

parentwd <- getwd()
outputwd <- "processing/enm/"
dir.create(outputwd, showWarnings = FALSE, recursive = TRUE)

# Bioclimatic variables
bio <- list.files("data/bioclim",
                  pattern = ".tif$",
                  full.names = TRUE)
bio <- stack(bio)

# gcm models
gcm <- list.dirs("data/gcm")[-1]
gcm <- do.call("rbind", strsplit(gcm, "/"))[,3]
gcm <- gsub("bi50","",gcm)

# define projection and extension #
myproj <- proj4string(bio)
myext  <- extent(bio) 
myres  <- res(bio)

# macs data
dt <- fread("data/macs.csv")
sp <- sort(unique(dt$acronym))
sp

# this allows for future loops if needed
i = 1

# Run Ensemble Modeling #
# create a dir for the species and work in that dir
output <- paste0(outputwd, sp[i])

dir.create(output, showWarnings = FALSE, recursive = TRUE)

setwd(output)

k <- dt$acronym == sp[i]

# subset data
coord <- dt[k, ]

message("\n Ensemble modelling for ", 
        unique(coord$acronym), "\n Time: ", 
        date(), "\n")

coord <- coord[, .(lon, lat)]

coord <- as.data.frame(coord)

# create background points over the area
nbg <- 200

bg <- randomPoints(bio[[1]], 
                   n = nbg, 
                   p = coord, 
                   ext = myext, 
                   extf = 1.25)

bg <- as.data.frame(bg)

names(bg) <- c("lon", "lat")

# reduce sampling bias removing points within the same grid cell
r <- raster(myext)

# set the resolution of the cells to ~ 30sec
res(r) <- myres

coord <- as.data.frame(coord)

# remove points in the same grid cell
coord <- gridSample(coord, r, n = 1)

# RUNNING AN ENSEMBLE MODELING
# STEP 1: model calibration

# here the function tests for the best algorithms since the algorithms were previously selected.
# A 3-fold cross-validation is performed to make sure that all pass the output.weights threshold

message("\n Step 1: Calibrating algorithms \n", "Time: ", date(), "\n")

set.seed(9999)

enm_step1 <- ensemble.calibrate.weights(x = bio, 
                                        p = coord, 
                                        a = bg,
                                        k = 5,
                                        layer.drops = NULL,
                                        SINK = TRUE, 
                                        species.name = sp[[i]],
                                        BIOCLIM = 1,
                                        MAHAL = 1,
                                        DOMAIN = 1, 
                                        MAXNET = 1,
                                        GBM = 1,
                                        GAM = 1,
                                        GLM = 1,
                                        SVM = 1,
                                        RPART = 1, 
                                        GBMSTEP = 1,
                                        MAXENT = 0, 
                                        NNET = 1, 
                                        RF = 1, 
                                        EARTH = 1,
                                        GLMSTEP = 1, 
                                        GAMSTEP = 1, 
                                        MGCV = 1, 
                                        MGCVFIX = 1, 
                                        CF = 1, 
                                        FDA = 1,
                                        SVME = 1,
                                        ENSEMBLE.tune = TRUE, 
                                        PROBIT = TRUE,
                                        # see Liu et al (2013) doi:10.1111/jbi.12058
                                        threshold.method = "threshold2013.mean", 
                                        threshold.PresenceAbsence = TRUE, 
                                        ENSEMBLE.min = 0.7)

# STEP 2: Create models that will be used for the raster predictions

# models with output.weights <0.05 are excluded
output_weights <- enm_step1$output.weights

output_weights[output_weights < 0.05] <- 0

message("Step 2: Model species distribution with selected ENM algorithms \n")

set.seed(9999)

enm_step2 <- ensemble.calibrate.models(x = bio, 
                                       p = coord, 
                                       a = bg,
                                       k = 5, 
                                       layer.drops = NULL,
                                       SINK = TRUE, 
                                       species.name = sp[[i]],
                                       models.keep = TRUE,
                                       input.weights = output_weights,
                                       # see Liu et al (2013) doi:10.1111/jbi.12058
                                       threshold.method = "threshold2013.mean", 
                                       threshold.PresenceAbsence = TRUE, 
                                       ENSEMBLE.tune = FALSE, 
                                       ENSEMBLE.min = 0.7,
                                       PROBIT = TRUE,
                                       Yweights = "BIOMOD", 
                                       models.save = FALSE)


# save AUCs
auc <- data.frame(auc = enm_step2$AUC.testing)

auc$model <- rownames(auc)

auc <- auc[!grepl("MAHAL|ENSEMBLE", rownames(auc)), ]

write.csv(auc, file = "outputs/auc_testing.csv")

message("Step 3.1: Generate map of current distribution \n")

#STEP 3: use previously calibrated models to construct consensus layers
ensemble_current <- ensemble.raster(xn = bio,
                                    models.list = enm_step2$models,
                                    input.weights = output_weights,
                                    thresholds = enm_step2$models$thresholds,
                                    SINK = TRUE,
                                    RASTER.species.name = sp[[i]], 
                                    RASTER.stack.name = "current")

# project suitability under climate change
for (j in seq_along(gcm)){
  message("Step 3.2: Predict future distribution, GCM ", toupper(gcm[j]), "\n")
  
  #load GCM layers
  gcmfiles <- list.files(paste0(parentwd, "/data/gcm/", gcm[[j]], "bi50"),
                         pattern = ".tif$",
                         full.names = TRUE)
  
  gcm_model <- stack(gcmfiles)

  crs(gcm_model) <- myproj
  
  ensemble_gcm_model <- ensemble.raster(xn = gcm_model,
                                        models.list = enm_step2$models,
                                        input.weights = output_weights,
                                        thresholds = enm_step2$models$thresholds,
                                        SINK = TRUE,
                                        RASTER.species.name = sp[i],
                                        RASTER.stack.name = gcm[j])
  
}

# remove working files created in the third step
unlink("models", recursive = TRUE)
unlink("ensembles/count", recursive = TRUE)
file.remove(list.files(pattern = "working", full.names = TRUE))

# return to parent wd
setwd(parentwd)

message("Done at ", Sys.time())