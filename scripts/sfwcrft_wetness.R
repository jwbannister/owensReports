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
b <- as.Date(substr(a, 0, 6), "%m%d%y")
swir_fl <- a[which(month(b)==month(start_date) & year(b)==year(start_date))][1]
swir_date <- as.Date(substr(swir_fl, 0, 6), format="%m%d%y")

if (!is.na(swir_fl)){
    # pull met data for precip table
    load_all("~/code/owensData")
    met_index <- data.frame(dca=c('T10-1', 'T29-2', 'T26', 'T1A-4'), 
                            deployment=c('1451', '1552', '1551', '1350'))
    query2 <- paste0("SELECT i.deployment, m.precip, ", 
                     "(m.datetime - '1 second'::interval) AS datetime, ", 
                     "(m.datetime - '1 second'::interval)::date AS date, ", 
                     "st_y(st_transform(i.geom, 26911)) AS northing_utm, ",
                     "st_x(st_transform(i.geom, 26911)) AS easting_utm, ",
                     "flags.is_invalid(m.deployment_id, 
                     (m.datetime - '1 hour'::interval), 
                     m.datetime) AS invalid ",
                     "FROM met.met_1hour m JOIN instruments.deployments i ",
                     "ON m.deployment_id = i.deployment_id ", 
                     "WHERE (datetime - '1 second'::interval)::date ",
                     "BETWEEN ('", swir_date, "'::date - '30 days'::interval)::date ",
                     "AND '", swir_date, "'::date ",
                     "AND i.deployment IN ('1552', '1551', '1451', '1350') ",
                     "ORDER BY i.deployment, datetime;")
met_df <- query_owens(query2) %>%
    left_join(met_index, by="deployment") %>%
    filter(!invalid) %>% group_by(date, dca) %>%
    summarize(pre=sum(precip), x=unique(easting_utm), y=unique(northing_utm)) %>%
    arrange(dca, date) %>% ungroup()
met_locs <- select(met_df, dca, x, y)
met_locs <- met_locs[!duplicated(met_locs), ]

if (!is.na(swir_fl)){
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
    wetness <- wetness %>% arrange(dca, as.numeric(gsub("%", "", trgtwet)))
    wetness$swir.wet <- paste0(wetness$swir.wet*100, "%")

    dcas <- vector(mode="list", length=length(unique(sfwcrft$data$dca)))
    names(dcas) <- unique(sfwcrft$data$dca)
    for (i in unique(sfwcrft$spdf@data$dcaid)){
        nm <- names(dcas)[i]
        print(paste0("Building raster: ", nm))
        temp <- raster::crop(dca_ras, 
                             sfwcrft$spdf[sfwcrft$spdf@data$dca==nm, ])
        temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
        reflect_ras <- ras_swir * temp
        dcas[[i]]$reflect_df <- as.data.frame(raster::rasterToPoints(reflect_ras))
        colnames(dcas[[i]]$reflect_df)[3] <- "value"
        dcas[[i]]$wet_ras <- class_wet(reflect_ras)
        dcas[[i]]$wet_df <- as.data.frame(raster::rasterToPoints(dcas[[i]]$wet_ras))
        colnames(dcas[[i]]$wet_df)[3] <- "value"
        dcas[[i]]$wet_df$value <- ordered(dcas[[i]]$wet_df$value, levels=c(1, 0), 
                                          labels=c("Wet", "Dry"))
        tmp_polys <- filter(sfwcrft$polygons, dca==nm) %>%
            select(x, y, id=treatment)
        tmp_labels <- filter(sfwcrft$labels, dca==nm) %>%
            select(x, y, id=treatment)
        background <- vector(mode="list", length=2)
        for (l in 1:2){
            if (l==1){
                tmp <- plot_dca_background_noboundaries(tmp_polys)
            } else{
                tmp <- plot_dca_background_noboundaries(tmp_polys, 
                                                        external_points=filter(met_locs, dca==nm)) 
            }
            background[[l]]$x_range <- ggplot_build(tmp)[[2]]$panel_ranges[[1]]$x.range
            background[[l]]$y_range <- ggplot_build(tmp)[[2]]$panel_ranges[[1]]$y.range
            background[[l]]$grob <- ggplotGrob(tmp)
        }

        dcas[[i]]$wet_plot <- ggplot(dcas[[i]]$wet_df, aes(x=x, y=y)) +
            annotation_custom(background[[1]]$grob, 
                              xmin=background[[1]]$x_range[1], 
                              xmax=background[[1]]$x_range[2], 
                              ymin=background[[1]]$y_range[1], 
                              ymax=background[[1]]$y_range[2]) +
geom_tile(aes(fill=value)) + 
geom_path(data=filter(sfwcrft$polygons, dca==nm),
          mapping=aes(x=x, y=y, group=objectid)) +
geom_label(data=filter(sfwcrft$labels, dca==nm), 
           mapping=aes(x=x, y=y, label=treatment)) +
coord_fixed() + xlim(background[[1]]$x_range) + ylim(background[[1]]$y_range) + 
scale_y_continuous(limits=background[[1]]$y_range, expand=c(0, 0)) + 
scale_x_continuous(limits=background[[1]]$x_range, expand=c(0, 0)) +
scale_fill_manual(name="Ground Condition", values=c("blue", "grey")) +
theme(axis.title=element_blank(), 
      axis.text=element_blank(),
      axis.ticks=element_blank(), 
      panel.background=NULL, 
      plot.title=element_text(hjust=0.5), 
      legend.position=leg_pos[[nm]],
      legend.background=element_rect(linetype="solid", color="black"), 
      legend.justification=leg_pos[[nm]]) +
ggtitle("Wetness Classification")

        dcas[[i]]$reflect_plot <- ggplot(dcas[[i]]$reflect_df, aes(x=x, y=y)) +
            annotation_custom(background[[2]]$grob, 
                              xmin=background[[2]]$x_range[1], 
                              xmax=background[[2]]$x_range[2], 
                              ymin=background[[2]]$y_range[1], 
                              ymax=background[[2]]$y_range[2]) +
geom_tile(aes(fill=value)) + 
geom_path(data=filter(sfwcrft$polygons, dca==nm),
          mapping=aes(x=x, y=y, group=objectid)) +
geom_label(data=filter(sfwcrft$labels, dca==nm), 
           mapping=aes(x=x, y=y, label=treatment)) +
coord_fixed() + xlim(background[[2]]$x_range) + ylim(background[[2]]$y_range) + 
scale_y_continuous(limits=background[[2]]$y_range, expand=c(0, 0)) + 
scale_x_continuous(limits=background[[2]]$x_range, expand=c(0, 0)) +
scale_fill_gradient(name="SWIR Reflectance", low="black", 
                    high="white") + 
geom_point(data=filter(met_locs, dca==nm), mapping=aes(color=dca), 
           size=6) +
scale_color_manual(name=NULL, values=c("red"), 
                   labels=c(paste0("Meteorological Station ", 
                                   filter(met_index, dca==nm)$deployment[1]))) +
theme(axis.title=element_blank(), 
      axis.text=element_blank(),
      axis.ticks=element_blank(), 
      plot.title=element_text(hjust=0.5), 
      panel.background=NULL, 
      legend.position=leg_pos[[nm]],
      legend.background=element_rect(linetype="solid", color="black"), 
      legend.justification=leg_pos[[nm]]) +
ggtitle("SWIR Reflectance")
        fl1 <- paste0(tempfile(), ".png")
png(filename=fl1, width=8, height=8, units="in", res=300)
print(dcas[[i]]$wet_plot)
dev.off()
fl2 <- paste0(tempfile(), ".png")
png(filename=fl2, width=8, height=8, units="in", res=300)
print(dcas[[i]]$reflect_plot)
dev.off()
wet_grob <- grid::rasterGrob(png::readPNG(fl1), interpolate=TRUE)
reflect_grob <- grid::rasterGrob(png::readPNG(fl2), interpolate=TRUE)
dcas[[i]]$img_file <- paste0(tempfile(), ".png")
png(filename=dcas[[i]]$img_file, width=8, height=4, units="in", res=300)
gridExtra::grid.arrange(reflect_grob, wet_grob, ncol=2)
dev.off()
    }
}
}
