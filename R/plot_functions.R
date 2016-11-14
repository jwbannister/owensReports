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
plot_csc_site <- function(background, sand_df, area_txt,
                            begin=start_date,
                            ending=end_date){
  catches <- sand_df %>% filter(dca==area_txt)
  mass.range <- range(catches$sand.mass)[2] - range(catches$sand.mass)[1]
  plot.title <- paste0(area_txt, " CSC Sites (", format(begin, "%m/%d"), 
                       " - ", format(ending, "%m/%d/%Y"), ")")
  x_range <- diff(ggplot_build(background)[[2]]$ranges[[1]]$x.range)
  y_range <- diff(ggplot_build(background)[[2]]$ranges[[1]]$y.range)
  catches$max.daily.flux <- sapply(catches$max.daily.flux, 
                                   function(x) ifelse(x>1, 1, x))
  p1 <- background +
    geom_point(data=catches, mapping=aes(x=x, y=y, color=max.daily.flux), 
               size=4) + 
    geom_label(data=catches, mapping=aes(x=x, y=y, label=csc), 
               nudge_x=x_range*0.06, nudge_y=y_range*0.03) +
    scale_color_gradientn(name='Max. Daily Flux\n(g/cm^2/day)\n', 
                          colors=c("green", "yellow", "red"), 
                          limits=c(-0.001, 1.001), 
                          breaks=c(0, 0.5, 1), 
                          labels=c("0", "0.5", ">1")) +
    coord_fixed() +
    ggtitle(plot.title) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=12))
  p1
}  

#' Plot DCA areas with background
#' 
#' @import dplyr
#' @import ggplot2
#' @param areas String.
#' @param polys_df Data frame. Points defining polygons are DCM areas. 
#' @param lables_df Data frame. Area labels and position coordinates.
#' @param external_points Data frame. *x* and *y* coordinates of points external 
#' to polygons to be included in plot range. 
plot_dca_background <- function(areas, polys_df, labels_df, 
                                external_points=NULL){
  polys <- polys_df %>% filter(area %in% areas)
  labels <- labels_df %>% filter(area %in% areas)
  plot.range <- get_plot_range(polys, external_points)
  map <- raster::stack("~/dropbox/owens/gis/pleiades/pleiades_20160824.tif")
  ext <- sp::SpatialPointsDataFrame(coords=cbind(x=plot.range$x, y=plot.range$y), 
                                data=data.frame(id=1:2), 
                                proj4string=raster::crs(map))
  map_sub <- raster::crop(map, raster::extent(ext))
  map_sub <- raster::aggregate(map_sub, 4)
  map_df <- raster::as.data.frame(map_sub, xy=T)
  map_df <- data.frame(x=map_df$x, y=map_df$y, r=map_df[ , 3], g=map_df[ , 4], 
                       b=map_df[ , 5])
  p1 <- ggplot(data=map_df) + coord_fixed() + theme_bw() +
  geom_tile(aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), alpha=0.75) + 
  geom_path(data=polys, mapping=aes(x=x, y=y, group=objectid), color="black") +
  geom_text(data=labels, aes(x=x, y=y, label=area), color="black") +
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

build_sfwcrft_areas <- function(){
    shpfile <- path.expand(paste0("~/dropbox/owens/gis/", 
                                  "sfwcrft 2015-16/", 
                                  "2015-2016 sfwcrft treatment areas/"))
    dat <- shape_data(shpfile, "DCM_SFWCT_2015_Bndys_070815", 
                      proj_string)
    sfwcrft_areas <- vector(mode="list", length=0)
    sfwcrft_areas$spdf <- rgdal::readOGR(shpfile, 
                                         "DCM_SFWCT_2015_Bndys_070815")
    sfwcrft_areas$spdf@data$dca <- sapply(sfwcrft_areas$spdf@data$DCM, rename)
    sfwcrft_areas$spdf@data$dcaid <- 
        sapply(sfwcrft_areas$spdf@data$dca, 
               function(x) which(unique(sfwcrft_areas$spdf@data$dca)==x))
    sfwcrft_areas$data <- dat
    sfwcrft_areas$data$dca <- sfwcrft_areas$data$dcm
    rename <- function(x){
        if (x=="T10") return("T10-1")
        if (x=="T13") return("T13-1")
        if (x=="T29") return("T29-2")
        if (x=="T26") return("T26")
        return(x)
    }
    sfwcrft_areas$data$dca <- sapply(sfwcrft_areas$data$dca, rename)
    sfwcrft_areas$polygons <- lists2df(dat, 8, 5) %>%
        left_join(select(sfwcrft_areas$data, objectid, area=treatment, dca), 
                  by="objectid")
    sfwcrft_areas$labels <- lists2df(dat, 7, 5) %>%
        left_join(select(sfwcrft_areas$data, objectid, area=treatment, dca), 
                  by="objectid")
    sfwcrft_areas
}

plot_csc_site_nolabel <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max){
  catches <- sand_df %>% filter(dca==area_txt)
  value.range <- 
      range(catches[ , value_index])[2] - range(catches[ , value_index])[1]
  plot.title <- paste0(area_txt, " CSC Sites (", format(begin, "%m/%d"), 
                       " - ", format(ending, "%m/%d/%Y"), ")")
  x_range <- diff(ggplot_build(background)[[2]]$ranges[[1]]$x.range)
  y_range <- diff(ggplot_build(background)[[2]]$ranges[[1]]$y.range)
  catches[ , value_index] <- 
      sapply(catches[ , value_index], 
             function(x) ifelse(x>value_max, value_max, x))
  p1 <- background +
    geom_point(data=catches, size=4,  
               mapping=aes_string(x='x', y='y', 
                                  color=names(sand_df)[value_index])) +
    scale_color_gradientn(name=legend_title,  
                          colors=c("green", "yellow", "red"), 
                          limits=c(-0.001, value_max+0.001), 
                          breaks=c(0, value_max/2, value_max), 
                          labels=c("0", as.character(value_max/2), 
                                   paste0(">", value_max))) +
    coord_fixed() +
    ggtitle(plot.title) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=12))
  p1
}  
