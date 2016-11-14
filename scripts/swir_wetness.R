load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(rgdal)
library(raster)
library(ggplot2)
library(tidyverse)
rasterOptions(tolerance = 1)

# read in SFWCRFT area shapefile
sfwcrft_areas <- build_sfwcrft_areas()

# build trimmed rasters in shape of areas for trimming SWIR images. 
ras_uniform <- raster("~/dropbox/data/swir/LS_adjusted/042616_LSadj.tif")
ras_uniform[ , ] <- 1
trgt_ras <- ras_clip(ras_uniform, sfwcrft_areas$spdf, 
                     fld="OBJECTID")
dca_ras <- ras_clip(ras_uniform, sfwcrft_areas$spdf, 
                    fld="dcaid")

# get list of file names for SWIR images
a <- list.files(path="~/dropbox/data/swir/LS_adjusted/", 
                pattern=".tif")
b <- as.Date(substr(a, 1, 6), "%m%d%y")
swir_fl <- a[which(month(b)==month(start_date) & year(b)==year(start_date))][1]

print(paste0("File used: ", swir_fl))
ras_swir <- raster(paste0("~/dropbox/data/swir/LS_adjusted/", swir_fl))

zones <- vector(mode="list", length=length(sfwcrft_areas$data$objectid))
names(zones) <- sfwcrft_areas$data$objectid
for (i in names(zones)){
    print(paste0("Building raster: ", i))
    temp <- trgt_ras
    temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
    zones[[i]] <- temp
}

wetness <- data.frame(dca=c(), trgtwet=c(), swir.wet=c())
for (j in names(zones)){
    print(paste0("Calculating wetness: ", j))
    dca_txt <- filter(sfwcrft_areas$data, objectid==j)$dca
    trgt_txt <- filter(sfwcrft_areas$data, objectid==j)$treatment
    reflect_ras <- ras_swir * zones[[j]]
    wet_ras <- class_wet(reflect_ras)
    wet_value <- sum(wet_ras[ , ], na.rm=T)/sum(!is.na(wet_ras[ , ]))
    temp <- data.frame(dca=dca_txt, trgtwet=trgt_txt, 
                       swir.wet=round(wet_value, 2))
    wetness <- rbind(wetness, temp)
}

dcas <- vector(mode="list", length=length(unique(sfwcrft_areas$data$dca)))
names(dcas) <- unique(sfwcrft_areas$data$dca)
for (i in unique(sfwcrft_areas$spdf@data$dcaid)){
    nm <- names(dcas)[i]
    print(paste0("Building raster: ", nm))
    temp <- dca_ras
    temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
    reflect_ras <- ras_swir * temp
#    dcas[[i]]$reflect_df <- as.data.frame(rasterToPoints(reflect_ras))
#    colnames(dcas[[i]]$reflect_df)[3] <- "value"
    dcas[[i]]$wet_ras <- class_wet(reflect_ras)
    dcas[[i]]$wet_df <- as.data.frame(rasterToPoints(dcas[[i]]$wet_ras))
    colnames(dcas[[i]]$wet_df)[3] <- "value"
    dcas[[i]]$wet_df$value <- ordered(dcas[[i]]$wet_df$value, levels=c(1, 0), 
                                      labels=c("Wet", "Dry"))
    dcas[[i]]$wet_plot <- ggplot(dcas[[i]]$wet_df, aes(x=x, y=y)) +
        geom_tile(aes(fill=value)) + 
        geom_path(data=filter(sfwcrft_areas$polygons, dca==nm),
                  mapping=aes(group=objectid)) +
        geom_label(data=filter(sfwcrft_areas$labels, dca==nm), 
                   mapping=aes(label=paste0("Target ", area))) +
        coord_fixed()
}

  
