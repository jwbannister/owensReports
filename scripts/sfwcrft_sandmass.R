load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)

if (start_date < "2016-08-01"){
    sfwcrft <- sfwcrft1
} else{
    sfwcrft <- sfwcrft2
}

sfwcrft$labels <- adjust_sfwcrft_labels()

daily_flux <- flux_df %>% 
    group_by(csc, date=date(datetime)) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2), 
              x=unique(easting_utm), y=unique(northing_utm),
              bad_coll=any(bad_coll)) %>% ungroup() 
csc_locs <- daily_flux[!duplicated(daily_flux$csc), ] %>%
    select(csc, x, y)
csc_locs$objectid <- apply(cbind(csc_locs$x, csc_locs$y), 1, 
                           owensMaps::point_in_dca, poly_df=sfwcrft$polygons)
csc_locs <- csc_locs %>% 
    left_join(select(sfwcrft$data, objectid, dca, treatment), by="objectid")
bad_collections <- daily_flux %>% group_by(csc) %>%
          summarize(x=unique(x), y=unique(y),
                    bad_count=sum(bad_coll), good_count=sum(!bad_coll)) %>%
          filter(bad_count>0) %>%
          left_join(select(csc_locs, csc, dca), by="csc")
bad_collections$flag <- sapply(bad_collections$good_count, function(x) 
                               if_else(x==0, "No Data For Month", 
                                       "Partial Data For Month"))
bad_collections$flag <- factor(bad_collections$flag)

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- flux_df %>% group_by(csc) %>% 
    summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1), 
              x=unique(easting_utm), y=unique(northing_utm)) %>% ungroup()
csc_mass$objectid <- apply(cbind(csc_mass$x, csc_mass$y), 1, 
                           owensMaps::point_in_dca, poly_df=sfwcrft$polygons)
csc_mass <- csc_mass %>% 
    left_join(select(sfwcrft$data, objectid, dca, treatment), by="objectid")
csc_mass$sand.mass <- sapply(csc_mass$sand.mass, 
                             function(x) ifelse(is.na(x), 0, x))
mass_ce <- calc_mass_ce_sfwcrft(csc_mass)
# FEB REPORT ONLY
# CHANGE CE to "-" SINCE WATER WAS OFF
mass_ce[mass_ce$dca=='T29-2', ]$control.eff <- "-"

mass_grobs <- vector(mode="list", length=length(unique(csc_mass$dca)))
names(mass_grobs) <- unique(csc_mass$dca)
for (i in unique(csc_mass$dca)){
    tmp_bad <- filter(bad_collections, dca==i & flag=="No Data For Month")
    tmp_partial <- filter(bad_collections, 
                          dca==i & flag=="Partial Data For Month")
    print(i)
    tmp_polys <- filter(sfwcrft$polygons, dca==i) %>%
        select(x, y, id=treatment)
    tmp_labels <- filter(sfwcrft$labels, dca==i) %>%
        select(x, y, id=treatment)
    background <- plot_dca_background(tmp_polys, tmp_labels) +
            geom_point(data=tmp_partial, mapping=aes(x=x, y=y), size=8, 
                       color="black")
    legend_mass='Total Mass (g)'
    p1 <- plot_csc_site(background, csc_mass, i, 
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


