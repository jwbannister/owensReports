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
#' @param polys_df 
#' @param labels_df 
#' @param external_points 
plot_dca_background <- function(polys_df, labels_df, 
                                external_points=NULL){
  plot.range <- owensMaps::get_plot_range(polys_df, external_points)
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
  geom_path(data=polys_df, mapping=aes(x=x, y=y, group=id), color="black") +
  geom_text(data=labels_df, aes(x=x, y=y, label=id), color="black") +
  scale_fill_identity() + 
  scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                     labels=range(map_df$x), expand = c(0,0)) +
  scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                     labels=range(map_df$y), expand = c(0,0)) +
  theme(panel.grid=element_blank())
p1
}

plot_csc_site_nolabel <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max, 
                            plot_title=""){
  catches <- sand_df %>% filter(dca==area_txt)
  value.range <- 
      range(catches[ , value_index])[2] - range(catches[ , value_index])[1]
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
    ggtitle(plot_title) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=12),
          legend.position=leg_pos[[area_txt]])
  p1
}  

leg_pos <- vector(mode="list", length=0)
leg_pos[['T10-1']] <- c(.8, .2)
leg_pos[['T26']] <- c(.2, .2)
leg_pos[['T13-1']] <- c(.2, .8)
leg_pos[['T29-2']] <- c(.2, .2)


