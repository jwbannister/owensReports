# sandmass_func.R
# John Bannister
# 
# Functions related to sand mass reporting for SFWCT areas.

#' Clean sand mass data from Owens Lake database
#' 
#' @param df_in Data frame. Sand mass data pulled from Owens Lake database with 
#' `Rowens::query_owenslake`.
#' @return Cleaned data frame of sand mass values. 
clean_sand <- function(df_in){
  df_in$collection_datetime <- as.Date(df_in$collection_datetime)
  df_in$collection <- paste0(lubridate::month(df_in$collection_datetime), "-", 
                             lubridate::year(df_in$collection_datetime))
  df_in$area <- gsub("-2", "", df_in$area)
  df_in$area <- gsub("-", "", df_in$area)

  df1 <- dplyr::select(df_in, collection.day=collection_datetime, 
                collection.month=collection, sand.mass=sandmass,
                collection.id=coll_id, csc.id=deployment, x=easting_utm, 
                y=northing_utm, dcm=area) 
  df1
}

#' Summarize SFWCT sand mass collection results
#' 
#' @import dplyr
#' @param df_in Data frame of sand mass data.
#' @return Data frame of sumamrized results.
summarize_sandmass <- function(df_in){
  df_in$treatment <- paste0("t_", substr(df_in$dcm, 5, nchar(df_in$dcm)-1))
  df_in$area <- substr(df_in$dcm, 1, 3)
  treat_sum <- df_in %>% group_by(area, treatment) %>% 
    summarize(avg.sand.mass=mean(sand.mass)) %>% ungroup()
  control_sum <- treat_sum %>% group_by(area) %>%
    do(control.mass=filter(., treatment=="t_0")$avg.sand.mass)
  control_sum[control_sum$area=="T13", 2] <- NA
  control_sum$control.mass <- unlist(control_sum$control.mass)
  treat_sum <- inner_join(treat_sum, control_sum, by="area") %>%
    mutate(control.eff=1-(avg.sand.mass/control.mass)) %>% 
    select(-control.mass)
  treat_sum$control.eff <- round(treat_sum$control.eff, 2) * 100
  treat_sum[treat_sum$treatment=="t_0", ]$control.eff <- NA
  treat_sum$treatment <- paste0(substring(treat_sum$treatment, 3), "%")
  treat_sum$avg.sand.mass <- round(treat_sum$avg.sand.mass, 2)
  treat_sum
}

#' Plot wind speeds prior to collection for SFWCT area.
#' 
#' @import dplyr
#' @import ggplot2
#' @param col_id Numeric. Collection id number. Wind speeds will be plotted from 
#' the end of the previous collection to the beginning of this collection.
#' @param area String. SFWCT area for which to plot wind speeds.
#' @param collections_df. Data frame. All sand collections made in SFWCT areas, 
#' with *collection.id, dcm, start, end* columns.
#' @return ggplot2 object. Plot of 5 minute 10m wind speeds over time between 
#' collections.
precollection_wind <- function(area,  
                               begin=start.date,
                               ending=end.date){
  wind.start <- begin
  wind.end <- ending
  met.index <- list("T10"="1451", "T13"="1452", "T26"="1551", "T29"="1552")
  query1 <- paste0(
                   "SELECT data.datetime, data.windspeed_10m, data.voltage,
                   inst.deployment, inst.description, data.winddirection_10m  
                   FROM mets.deployment_data data 
                   JOIN instruments.deployments inst 
                   ON inst.deployment_id=data.deployment_id 
                   WHERE inst.deployment='", met.index[area], 
                   "' AND data.datetime > '", wind.start,
                   "' AND data.datetime < '", wind.end, "'")
  df2 <- query_owenslake(query1)
  df2 <- filter(df2, windspeed_10m<500)
  p1 <- ggplot(df2, aes(x=datetime, y=windspeed_10m)) +
    geom_path() + 
    ggtitle(area) +
    ylab("10m Wind Speed (m/s)") + xlab("")
  p1
}

#' Plot SFWCT area with CSC masses for collection
#' 
#' @import dplyr
#' @import ggplot2
#' @param background GGplot object. Background image with treatment borders and 
#' labels.
#' @param sand_df Data frame of sand mass data.
#' @param area. String. Area to be plotted.
#' @return ggplot2 object. Plot of SFWCT area with sand catch masses and 
#' countour lines.
plot_csc_masses <- function(background, sand_df, area_txt,
                            begin=start.date,
                            ending=end.date){
  sand_df$area <- substr(sand_df$dcm, 1, 3)
  if (!is.na(area_txt)) catches <- sand_df %>% filter(area==area_txt)
  mass.range <- range(catches$sand.mass)[2] - range(catches$sand.mass)[1]
  df1 <- akima::interp(catches$x, catches$y, catches$sand.mass)
  df2 <- reshape2::melt(df1$z, na.rm=TRUE)
  names(df2) <- c("x.ind", "y.ind", "sand.mass")
  df2$x <- df1$x[df2$x.ind]
  df2$y <- df1$y[df2$y.ind]
  plot.title <- paste0(area_txt, " Sand Mass Contours (", 
                       format(begin, "%m/%d"), " - ", 
                       format(ending, "%m/%d/%Y"), ")")
leg.pos <- list("T26"=c(0.15, 0.2),
                "T10"=c(0.85, 0.2),
                "T13"=c(0.15, 0.8),
                "T29"=c(0.15, 0.2))
  p1 <- background +
    geom_point(data=catches, mapping=aes(x=x, y=y, color=sand.mass, 
                                         shape="CSC Site")) +
    coord_fixed() +
    ggtitle(plot.title) +
    stat_contour(data=df2, binwidth=mass.range/5,
                   mapping=aes(x=x, y=y, z=sand.mass, 
                               color=..level..)) +
    scale_colour_gradientn("Sand Mass (g)", limits=range(sand_df$sand.mass),  
                           colours=c("darkgreen", "yellow", "red"))+
    labs(shape=NULL) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position=leg.pos[[area_txt]],
          legend.background=element_rect(color="black"),
          legend.box.just="left", 
          plot.title=element_text(size=12))
  p1
}  

#' Plot DCM areas with background
#' 
#' @import dplyr
#' @import ggplot2
#' @param areas String.
#' @param polys_df Data frame. Points defining polygons are DCM areas. 
#' @param lables_df Data frame. Area labels and position coordinates.
#' @param external_points Data frame. *x* and *y* coordinates of points external 
#' to polygons to be included in plot range. 
plot_dcm_background <- function(areas, polys_df, labels_df, 
                                external_points=NULL){
  polys <- polys_df %>%
    inner_join(select(labels_df, objectid, dcm), by="objectid") %>%
    filter(dcm %in% areas)
  labels <- labels_df %>% filter(dcm %in% areas)
  plot.range <- get_plot_range(polys, external_points)
  map <- raster::stack("~/dropbox/data/dustReport/data-raw/owens_background.tif")
  ext <- sp::SpatialPointsDataFrame(coords=cbind(x=plot.range$x, y=plot.range$y), 
                                data=data.frame(id=1:2), 
                                proj4string=raster::crs(map))
  map_sub <- raster::crop(map, raster::extent(ext))
  map_sub <- raster::aggregate(map_sub, 4)
  map_df <- raster::as.data.frame(map_sub, xy=T)
  map_df <- data.frame(x=map_df$x, y=map_df$y, r=map_df$owens_background.1, 
                       g=map_df$owens_background.2, b=map_df$owens_background.3)
  p1 <- 
  ggplot(data=map_df) + 
  coord_fixed() + 
  theme_bw() +
  geom_tile(aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), alpha=0.75) + 
  geom_path(data=polys, mapping=aes(x=x, y=y, group=objectid), color="black") +
  geom_text(data=labels, aes(x=x, y=y, label=label), color="black") +
  scale_fill_identity() + 
  scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                     labels=range(map_df$x), expand = c(0,0)) +
  scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                     labels=range(map_df$y), expand = c(0,0)) +
  theme(panel.grid=element_blank())
p1
}


#' Get coordinate ranges for square plot around DCM areas.
#' 
#' @param polys. Data frame. Points defining polygons of interest.
#' @param external_points Data frame. *x* and *y* coordinates of points external 
#' to polygons to be included in plot range. 
#' @return A list with the x and y ranges for a sqaure plot around the areas of 
#' interest.
get_plot_range <- function(polys, external_points=NULL){
  p.temp <- ggplot(polys, aes(x=x, y=y)) + geom_path() +
    geom_point(data=external_points)
  info <- ggplot_build(p.temp)
  plot_xrange <- info[[2]]$ranges[[1]]$x.range
  plot_yrange <- info[[2]]$ranges[[1]]$y.range
  maxspan <- max(c(plot_xrange[2] - plot_xrange[1], 
                   plot_yrange[2] - plot_yrange[1]))
  midpoint <- c(mean(plot_xrange), mean(plot_yrange))
  xrange <- c(midpoint[1] - (maxspan/2), 
              midpoint[1] + (maxspan/2))
  yrange <- c(midpoint[2] - (maxspan/2), 
              midpoint[2] + (maxspan/2))
  plot.range <- list(x=xrange, y=yrange)
  plot.range
}
 
clean_ce <- function(x){
  x <- as.character(x)
  x <- sapply(x, function(x) ifelse((x=="Inf" | x=="-Inf" | is.na(x) 
                                     | x=="NaN"), 
                                    "NA", paste0(x, "%")))
}
