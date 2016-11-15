load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)
raster::rasterOptions(tolerance = 1)

if (start_date < "2016-08-01"){
    sfwcrft <- sfwcrft1
} else{
    sfwcrft <- sfwcrft2
}

# build trimmed rasters in shape of areas for trimming SWIR images. 
ras_uniform <- 
    raster::raster("~/dropbox/data/swir/LS_adjusted/042616_LSadj.tif")
ras_uniform[ , ] <- 1
sfwcrft$spdf@data$dcaid <- 
    sapply(sfwcrft$spdf@data$dca, 
           function(x) which(unique(sfwcrft$spdf@data$dca)==x))
trgt_ras <- ras_clip(ras_uniform, sfwcrft$spdf, fld="objectid")
dca_ras <- ras_clip(ras_uniform, sfwcrft$spdf, fld="dcaid")

# get list of file names for SWIR images
a <- list.files(path="~/dropbox/data/swir/LS_adjusted/", 
                pattern=".tif")
b <- as.Date(substr(a, 1, 6), "%m%d%y")
swir_fl <- a[which(month(b)==month(start_date) & year(b)==year(start_date))][1]

print(paste0("File used: ", swir_fl))
ras_swir <- raster::raster(paste0("~/dropbox/data/swir/LS_adjusted/", swir_fl))

zones <- vector(mode="list", length=length(sfwcrft$data$objectid))
names(zones) <- sfwcrft$data$objectid
for (i in names(zones)){
    print(paste0("Building raster: ", i, "/", length(names(zones))))
    temp <- raster::crop(trgt_ras, 
                         sfwcrft$spdf[sfwcrft$spdf@data$objectid==i, ])
    temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
    zones[[i]] <- temp
}

wetness <- data.frame(dca=c(), trgtwet=c(), swir.wet=c())
for (j in names(zones)){
    print(paste0("Calculating wetness: ", j, "/", length(names(zones))))
    dca_txt <- filter(sfwcrft$data, objectid==j)$dca
    trgt_txt <- filter(sfwcrft$data, objectid==j)$treatment
    reflect_ras <- ras_swir * zones[[j]]
    wet_ras <- class_wet(reflect_ras)
    wet_value <- sum(wet_ras[ , ], na.rm=T)/sum(!is.na(wet_ras[ , ]))
    temp <- data.frame(dca=dca_txt, trgtwet=trgt_txt, 
                       swir.wet=round(wet_value, 2))
    wetness <- rbind(wetness, temp)
}

dcas <- vector(mode="list", length=length(unique(sfwcrft$data$dca)))
names(dcas) <- unique(sfwcrft$data$dca)
for (i in unique(sfwcrft$spdf@data$dcaid)){
    nm <- names(dcas)[i]
    print(paste0("Building raster: ", nm))
    temp <- raster::crop(dca_ras, 
                         sfwcrft$spdf[sfwcrft$spdf@data$dca==nm, ])
    temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
    reflect_ras <- ras_swir * temp
#    dcas[[i]]$reflect_df <- as.data.frame(rasterToPoints(reflect_ras))
#    colnames(dcas[[i]]$reflect_df)[3] <- "value"
    dcas[[i]]$wet_ras <- class_wet(reflect_ras)
    dcas[[i]]$wet_df <- as.data.frame(raster::rasterToPoints(dcas[[i]]$wet_ras))
    colnames(dcas[[i]]$wet_df)[3] <- "value"
    dcas[[i]]$wet_df$value <- ordered(dcas[[i]]$wet_df$value, levels=c(1, 0), 
                                      labels=c("Wet", "Dry"))
    dcas[[i]]$wet_plot <- ggplot(dcas[[i]]$wet_df, aes(x=x, y=y)) +
        geom_tile(aes(fill=value)) + 
        geom_path(data=filter(sfwcrft$polygons, dca==nm),
                  mapping=aes(group=objectid)) +
        geom_label(data=filter(sfwcrft$labels, dca==nm), 
                   mapping=aes(label=paste0("Target ", treatment))) +
        coord_fixed() +
        scale_fill_manual(name="", values=c("blue", "grey")) +
        theme(axis.title=element_blank(), 
              axis.text=element_blank(),
              axis.ticks=element_blank(), 
              panel.background=NULL, 
              legend.position=leg_pos[[nm]]) +
        ggtitle("SWIR Wet/Dry Image")
}

  
