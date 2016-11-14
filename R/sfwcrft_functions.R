
build_sfwcrft_areas <- function(){
    rename <- function(x){
        if (x=="T10") return("T10-1")
        if (x=="T13") return("T13-1")
        if (x=="T29") return("T29-2")
        if (x=="T26") return("T26")
        return(x)
    }
    shpfile <- 
        path.expand(paste0("~/dropbox/owens/gis/sfwcrft 2015-16/", 
                           "2015-2016 sfwcrft treatment areas/"))
    dat <- shape_data(shpfile, "DCM_SFWCT_2015_Bndys_070815", proj_string)
    sfwcrft_areas <- vector(mode="list", length=0)
    sfwcrft_areas$spdf <- rgdal::readOGR(shpfile, 
                                         "DCM_SFWCT_2015_Bndys_070815")
    sfwcrft_areas$spdf@data$dca <- sapply(sfwcrft_areas$spdf@data$DCM, rename)
    sfwcrft_areas$spdf@data$dcaid <- 
        sapply(sfwcrft_areas$spdf@data$dca, 
               function(x) which(unique(sfwcrft_areas$spdf@data$dca)==x))
    sfwcrft_areas$data <- dat
    sfwcrft_areas$data$dca <- sfwcrft_areas$data$dcm
    sfwcrft_areas$data$dca <- sapply(sfwcrft_areas$data$dca, rename)
    sfwcrft_areas$polygons <- lists2df(dat, 8, 5) %>%
        left_join(dplyr::select(sfwcrft_areas$data, objectid, area=treatment, dca), 
                  by="objectid")
    sfwcrft_areas$labels <- lists2df(dat, 7, 5) %>%
        left_join(dplyr::select(sfwcrft_areas$data, objectid, area=treatment, dca), 
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

shape_data <- function(dsn, layer, proj_string){
  dsn <- path.expand(dsn)
  areas <- rgdal::readOGR(dsn=dsn, layer=layer, verbose=FALSE)
  areas <- sp::spTransform(areas, proj_string)
  dat <- areas@data 
  labpnts <- lapply(c(1:length(areas@polygons)), 
                    function(x) areas@polygons[[x]]@labpt)
  polypnts <- lapply(c(1:length(areas@polygons)), 
                     function(x) areas@polygons[x][[1]]@Polygons[[1]]@coords)
  area_data <- cbind(dat, I(labpnts), I(polypnts)) 
  colnames(area_data) <- tolower(colnames(area_data))
  area_data
}
