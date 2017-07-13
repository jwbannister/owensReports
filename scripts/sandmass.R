load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)

daily_flux <- flux_df %>% filter(!invalid) %>%
    group_by(csc, date=date(datetime)) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2), 
              x=unique(easting_utm), y=unique(northing_utm),
              bad_coll=any(bad_coll)) %>% ungroup() 

bad_collections <- daily_flux %>% group_by(csc) %>%
    summarize(x=unique(x), y=unique(y),
              bad_count=sum(bad_coll), good_count=sum(!bad_coll)) %>%
    filter(bad_count>0) %>%
    left_join(select(csc_locs, csc, id1, id2, id3), by="csc")
bad_collections$flag <- sapply(bad_collections$good_count, function(x) 
                               if_else(x==0, "No Data For Month", 
                                       "Partial Data For Month"))
if (nrow(bad_collections)>0){
    bad_collections$flag <- factor(bad_collections$flag)
}
if (nrow(bad_collections)==0) bad_collections[1, 1:ncol(bad_collections)] <- 0

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- flux_df %>% filter(!invalid) %>% group_by(csc) %>% 
    summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1), 
              x=unique(easting_utm), y=unique(northing_utm)) %>% ungroup()
csc_mass$objectid <- apply(cbind(csc_mass$x, csc_mass$y), 1, 
                           owensMaps::point_in_dca, poly_df=area_polys)
csc_mass <- csc_mass %>% 
    left_join(select(area_data, objectid, id1, id2, id3), by="objectid")
csc_mass$sand.mass <- sapply(csc_mass$sand.mass, 
                             function(x) ifelse(is.na(x), 0, x))
if (area=='sfwcrft') mass_ce <- calc_mass_ce_sfwcrft(csc_mass)

# One-off adjustments specific to report
# CHECK EACH MONTH AND CHANGE IF NECESSARY
change_file <- paste0("~/code/owensReports/data/changes/", area, 
                      month(start_date), year(start_date), ".R")
if (file.exists(change_file)) source(change_file)

if (!(area %in% c('twb2', 'sfwcrft'))){
    area_labels <- owens$labels %>% filter(objectid %in% csc_locs$objectid) %>% 
        rename(id2=dca) %>% mutate(id1=id2, id3=id2)
    if (area=="dwm") area_labels <- move_dwm_labels(area_labels)
    if (area=="brine") area_labels <- move_brine_labels(area_labels)
    if (area=="channel") area_labels <- move_channel_labels(area_labels)
    if (area=="t1a1") area_labels <- move_t1a1_labels(area_labels)
}

if (area=='sfwcrft'){
    mass_grobs <- vector(mode="list", length=length(unique(csc_mass$id2)))
    names(mass_grobs) <- unique(csc_mass$id2)
    for (i in unique(csc_mass$id2)){
        print(i)
        tmp_bad <- filter(bad_collections, id2==i & flag=="No Data For Month")
        tmp_partial <- filter(bad_collections, 
                              id2==i & flag=="Partial Data For Month")
        tmp_polys <- filter(area_polys, id2==i)
        tmp_labels <- filter(area_labels, id2==i)
        background <- plot_dca_background(tmp_polys, tmp_labels) +
            geom_point(data=tmp_partial, mapping=aes(x=x, y=y), size=8, 
                       color="black")
        legend_mass='Total Mass (g)'
        tmp_mass <- filter(csc_mass, id2==i)
        p1 <- plot_csc_site(background, tmp_mass, i, 
                            legend_title=legend_mass, 
                            value_index=2, value_max=200,
                            plot_title="Monthly Mass") +
geom_point(data=tmp_partial, mapping=aes(x=x, y=y, shape=flag), 
           color="black", size=6) +
scale_shape_manual(name=NULL, values=c(21)) +
geom_point(data=tmp_bad, mapping=aes(x=x, y=y, size=flag), 
           color="black") +
scale_size_manual(name=NULL, values=c(4)) +
guides(color=guide_colorbar(order=1), shape=guide_legend(order=2), 
       size=guide_legend(order=3))
fl <- tempfile()
png(filename=fl, width=8, height=8, units="in", res=300)
print(p1)
dev.off()
mass_plot <- png::readPNG(fl)
mass_grobs[[i]] <- grid::rasterGrob(mass_plot, interpolate=TRUE)
    }
}


