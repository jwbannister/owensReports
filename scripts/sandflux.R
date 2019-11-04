library(ggmap)
google_key = Sys.getenv("OWENS_MAPS_KEY")

# add met station locations if needed for plot display
met_loc <- NULL
#met_loc <- data.frame(id3=c('North', 'South'), deployment=c('1552', '1150'), 
#                        x=c(415077.3, 409413.6), y=c(4041263.2, 4019839.6))

hourly_flux <- full_flux %>% filter((!invalid | is.na(invalid)) & 
                                   (!bad_coll | is.na(bad_coll))) %>%
    group_by(csc, date=as.Date(datetime %m-% seconds(1),
                               tz='America/Los_Angeles'),
             hour=hour(datetime %m-% seconds(1))+1) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2)) %>%
    left_join(csc_locs, by="csc") %>%
    ungroup() 

daily_flux <- full_flux %>% filter((!invalid | is.na(invalid)) & 
                                   (!bad_coll | is.na(bad_coll))) %>%
    group_by(csc, date=as.Date(datetime %m-% seconds(1),
                               tz='America/Los_Angeles')) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2)) %>%
    left_join(csc_locs, by="csc") %>%
    ungroup() 
split_dcas <- strsplit(unique(gsub("T", "", csc_mass$id3)), "-")
dca_break <- list(sapply(split_dcas, function(x) strtoi(parse_dca(x[1])[1])), 
    sapply(split_dcas, function(x) parse_dca(x[1])[2]), 
    sapply(split_dcas, function(x) strtoi(parse_dca(x[2])[1])), 
    sapply(split_dcas, function(x) parse_dca(x[2])[2]))
report_index <- unique(csc_mass$id3)[order(dca_break[[1]], dca_break[[2]], 
                                           dca_break[[3]], dca_break[[4]])]
if (area=='twb2'){
    report_index <- c('South', 'Central', 'East')
}

max_daily <- daily_flux %>% group_by(csc) %>%
    summarize(max.daily.flux = round(max(sand.flux), 2), x=unique(x), y=unique(y),
              id2=unique(id2), id3=unique(id3), id1=unique(id1)) 

flux_grobs <- vector(mode="list", length=length(report_index))
names(flux_grobs) <- report_index
for (i in report_index){
    print(i)
    tmp_bad <- filter(bad_collections, id3==i & flag=="No Data For Month")
    tmp_partial <- filter(bad_collections, 
                          id3==i & flag=="Partial Data For Month")
    tmp_polys <- filter(area_polys, id3==i)
    tmp_labels <- filter(area_labels, id3==i)
    tmp_flux <- filter(max_daily, id3==i)
    if (nrow(tmp_flux)==0){
        tmp_flux <- data.frame(csc=tmp_bad$csc, max.daily.flux=rep(0, nrow(tmp_bad)), 
                               x=tmp_bad$x, y=tmp_bad$y, id1=tmp_bad$id1, 
                               id2=tmp_bad$id2, id3=tmp_bad$id3)
    }
    if (is.null(met_loc)){
        met_pts <- c()
    } else{
        met_pts <- filter(met_loc, id3==i)
    }
    plot_range_x <- range(c(met_pts$x, tmp_polys$x))
    plot_range_y <- range(c(met_pts$y, tmp_polys$y))
    extent <- data.frame(x=extendrange(r=plot_range_x, f=0.3),
                         y=extendrange(r=plot_range_y, f=0.3))
    background <- photo_background(extent$x[1], extent$x[2], 
                                   extent$y[1], extent$y[2], 
                                   zone="11N", key=google_key, trim=TRUE) +
    geom_path(data=tmp_polys, mapping=aes(x=x, y=y, group=objectid), 
              color="black") +
    geom_text(data=tmp_labels, aes(x=x, y=y, label=id1), color="black") 
    geom_point(data=tmp_partial, mapping=aes(x=x, y=y), size=8, 
               color="black") 
    label_space_x <- 
        diff(ggplot_build(background)$layout$panel_params[[1]]$x.range)
    label_space_y <- 
        diff(ggplot_build(background)$layout$panel_params[[1]]$y.range)
    top_flux <- if_else(area=='twb2', 1, 10)
    label_df <- rbind(select(tmp_flux, csc, x, y, id1, id2, id3), 
                      select(tmp_bad, csc, x, y, id1, id2, id3)) %>%
        distinct()
    if (area %in% c('channel', 't1a1')){
        clr_bool = FALSE
        legend_flux = ""
        plot_title = "Monitoring Sites"
    } else{
        clr_bool = TRUE
        legend_flux='Max. Daily Flux\n(g/cm^2/day)'
        plot_title = "Daily Flux"
    }
    p1 <- plot_csc_site(background, tmp_flux, i, legend_title=legend_flux, 
                        value_index=2, value_max=top_flux, plot_title=plot_title,
                        labels=FALSE, colorscale=clr_bool) +
        geom_point(data=tmp_partial, mapping=aes(x=x, y=y, shape=flag), 
                   color="black", size=6) +
        scale_shape_manual(name=NULL, values=c(21)) +
        geom_point(data=tmp_bad, mapping=aes(x=x, y=y, size=flag), 
                   color="black") +
#        ggrepel::geom_label_repel(data=label_df, mapping=aes(x=x, y=y, label=csc), 
#                   nudge_x=label_space_x/25, nudge_y=label_space_y/45,
#                   box.padding=0.5) +
        scale_size_manual(name=NULL, values=c(4)) +
        guides(color=guide_colorbar(order=1), shape=guide_legend(order=2), 
               size=guide_legend(order=3))
    if (area %in% c('channel', 't1a1')){
        p1 <- p1 + geom_label(data=label_df, mapping=aes(x=x, y=y, label=csc),
                   nudge_x=label_space_x/45, nudge_y=label_space_y/40)
    } else{
        p1 <- p1 + 
        ggrepel::geom_label_repel(data=label_df, mapping=aes(x=x, y=y, label=csc), 
                   nudge_x=label_space_x/25, nudge_y=label_space_y/45,
                   box.padding=0.5)
    }
    if (!is.null(met_pts)){
         p1 <- p1 + geom_point(data=met_pts, 
                               aes(shape=deployment, x=x, y=y), 
                               color="blue", size=6) +
                scale_shape_manual(name='Met Station', values=c(17), 
                                   labels=c('1552')) 
    }
    fl <- tempfile(fileext=".png")
    png(filename=fl, width=8, height=8, units="in", res=300)
    print(p1)
    dev.off()
    flux_plot <- png::readPNG(fl)
    flux_grobs[[i]] <- grid::rasterGrob(flux_plot, interpolate=TRUE)
}
