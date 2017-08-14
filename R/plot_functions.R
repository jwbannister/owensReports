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
  map <- raster::stack("~/gis/owens/report_background.tif")
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
  geom_path(data=polys_df, mapping=aes(x=x, y=y, group=objectid), color="black") +
  geom_text(data=labels_df, aes(x=x, y=y, label=id1), color="black") +
  scale_fill_identity() + 
  scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                     labels=range(map_df$x), expand = c(0,0)) +
  scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                     labels=range(map_df$y), expand = c(0,0)) +
  theme(panel.grid=element_blank(), 
        axis.title=element_blank(), 
        plot.title=element_text(hjust=0.5))
p1
}

plot_csc_site_nolabel <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max, 
                            plot_title=""){
  x_range <- diff(ggplot_build(background)[[2]]$panel_ranges[[1]]$x.range)
  y_range <- diff(ggplot_build(background)[[2]]$panel_ranges[[1]]$y.range)
  sand_df[ , value_index] <- 
      sapply(sand_df[ , value_index], 
             function(x) ifelse(x>value_max, value_max, x))
  p1 <- background +
    geom_point(data=sand_df, size=4,  
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
          plot.title=element_text(size=12, hjust=0.5),
          legend.position=leg_pos[[area_txt]], 
          legend.background=element_rect(linetype="solid", color="black"), 
          legend.justification=leg_pos[[area_txt]])
  p1
}  

plot_dca_background_noboundaries <- function(polys_df, external_points=NULL){
  plot.range <- owensMaps::get_plot_range(polys_df, external_points)
  map <- raster::stack("~/gis/owens/report_background.tif")
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
  scale_fill_identity() + 
  scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                     labels=range(map_df$x), expand = c(0,0)) +
  scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                     labels=range(map_df$y), expand = c(0,0)) +
  theme(panel.grid=element_blank(), 
        axis.title=element_blank())
p1
}

plot_csc_site <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max, 
                            plot_title=""){
  x_range <- diff(ggplot_build(background)[[2]]$panel_ranges[[1]]$x.range)
  y_range <- diff(ggplot_build(background)[[2]]$panel_ranges[[1]]$y.range)
  sand_df[ , value_index] <- 
      sapply(sand_df[ , value_index], 
             function(x) ifelse(x>value_max, value_max, x))
  p1 <- background +
    geom_point(data=sand_df, size=4,  
               mapping=aes_string(x='x', y='y', 
                                  color=names(sand_df)[value_index])) +
    scale_color_gradientn(name=legend_title, 
                          colors=c("green", "yellow", "red"), 
                          limits=c(-0.001, value_max+0.001), 
                          breaks=c(0, value_max/2, value_max), 
                          labels=c("0", as.character(value_max/2), 
                                   paste0(">", value_max))) +
    coord_fixed() +
    ggrepel::geom_label_repel(data=sand_df, mapping=aes(x=x, y=y, label=csc), 
               nudge_x=x_range/25, nudge_y=y_range/45) +
    ggtitle(plot_title) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=12, hjust=0.5),
          legend.position=leg_pos[[area_txt]], 
          legend.background=element_rect(linetype="solid", color="black"), 
          legend.justification=leg_pos[[area_txt]])
  p1
}  

# plot wetness maps for sfwcrft
plot_wetness <- function(df1, background_df, type){
    if (type=='wet'){
        fill_title='Ground Condition' 
        fill_values=c('blue', 'grey')
        plot_title='Wetness Classification' 
    } else{
        fill_title='SWIR Reflectance' 
        fill_values=c('black', 'white')
        plot_title='SWIR Reflectance' 
    }
    p1 <- ggplot(df1, aes(x=x, y=y)) +
        annotation_custom(background_df$grob, 
                          xmin=background_df$x_range[1], 
                          xmax=background_df$x_range[2], 
                          ymin=background_df$y_range[1], 
                          ymax=background_df$y_range[2]) +
        geom_tile(aes(fill=value)) + 
        geom_path(data=filter(sfwcrft$polygons, dca==nm),
                  mapping=aes(x=x, y=y, group=objectid), 
                  color="darkorange") +
        geom_label(data=filter(sfwcrft$labels, dca==nm), 
                   mapping=aes(x=x, y=y, label=treatment)) +
        coord_fixed() + 
        xlim(background_df$x_range) + ylim(background_df$y_range) + 
        scale_y_continuous(limits=background_df$y_range, expand=c(0, 0)) + 
        scale_x_continuous(limits=background_df$x_range, expand=c(0, 0)) +
        theme(axis.title=element_blank(), 
              axis.text=element_blank(),
              axis.ticks=element_blank(), 
              panel.background_df=NULL, 
              plot.title=element_text(hjust=0.5), 
              legend.position=leg_pos[[nm]],
              legend.background=element_rect(linetype="solid", color="black"), 
              legend.justification=leg_pos[[nm]]) +
        ggtitle(plot_title)
    if (type=='wet'){
        p1 <- p1 + scale_fill_manual(name=fill_title, values=fill_values) 
    } else{
        p1 <- p1 + scale_fill_gradient(name="SWIR Reflectance", 
                                       low=fill_values[1], high=fill_values[2])
    }
}

# adjust legend positions for plotting
leg_pos <- vector(mode="list", length=0)
leg_pos[['T10-1']] <- c(1, 0)
leg_pos[['T26']] <- c(0, 0)
leg_pos[['T13-1']] <- c(.2, .8)
leg_pos[['T29-2']] <- c(0, 0)
leg_pos[['T1A-4']] <- c(1, 0)
leg_pos[['T2-4']] <- c(1, 0)
leg_pos[['T2-5']] <- c(1, 0)
leg_pos[['T8W']] <- c(1, 0)
leg_pos[['T10-3W']] <- c(1, 0)
leg_pos[['T23NE']] <- c(1, 0)
leg_pos[['T27 Addition']] <- c(0, 0)
leg_pos[['T29-4']] <- c(1, 0)
leg_pos[['T36-3 Addition']] <- c(1, 0)
leg_pos[['T36-3E']] <- c(1, 0)
leg_pos[['T36-2W']] <- c(1, 0)
leg_pos[['T1-1']] <- c(1, 0)
leg_pos[['T5-3']] <- c(1, 0)
leg_pos[['T5-3 Addition']] <- c(1, 0)
leg_pos[['T9']] <- c(1, 0)
leg_pos[['T10-2S']] <- c(1, 0)
leg_pos[['T10-2N']] <- c(1, 0)
leg_pos[['T10-3E']] <- c(1, 0)
leg_pos[['T18-0']] <- c(1, 0)
leg_pos[['T21W']] <- c(1, 0)
leg_pos[['T25-3']] <- c(1, 0)
leg_pos[['T37-2']] <- c(1, 0)
leg_pos[['North']] <- c(0, 1)
leg_pos[['Central']] <- c(0, 1)
leg_pos[['East']] <- c(0, 0)
leg_pos[['South']] <- c(0, 1)
leg_pos[['C1']] <- c(1, 0)
leg_pos[['C2']] <- c(1, 0)
leg_pos[['T1A-1']] <- c(1, 0)
leg_pos[['T16']] <- c(1, 0)
leg_pos[['T29-3']] <- c(0, 0)
leg_pos[['T1-1']] <- c(1, 1)

