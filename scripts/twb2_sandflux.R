load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)

twb2 <- group_twb2_areas()

daily_flux <- flux_df %>% 
    group_by(csc, date=date(datetime)) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2), 
              x=unique(easting_utm), y=unique(northing_utm)) %>% ungroup() 
    csc_locs <- daily_flux[!duplicated(daily_flux$csc), ] %>%
        select(csc, x, y)
    csc_locs$objectid <- apply(cbind(csc_locs$x, csc_locs$y), 1, 
                               owensMaps::point_in_dca, poly_df=twb2$polygons)
    csc_locs <- csc_locs %>% 
        left_join(select(twb2$data, objectid, dca, group), by="objectid")
    expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                                date=seq(start_date, end_date, "days"), 
                                stringsAsFactors=FALSE)
    full_daily <- expand_daily %>%
        left_join(select(daily_flux, -x, -y), by=c("csc", "date")) %>%
        left_join(csc_locs, by="csc")
    full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0

    max_daily <- full_daily %>% group_by(csc) %>%
        summarize(max.daily.flux = max(sand.flux), x=unique(x), y=unique(y),
                  dca=unique(dca), group=unique(group)) 

    flux_grobs <- vector(mode="list", length=length(unique(max_daily$group)))
    names(flux_grobs) <- unique(max_daily$group)
    for (i in unique(max_daily$group)){
        print(i)
        tmp_polys <- filter(twb2$polygons, group==i) %>%
            select(x, y, id=objectid)
        tmp_labels <- filter(twb2$labels, group==i) %>%
            select(x, y, id=dca)
    tmp_flux <- filter(max_daily, group==i)
    tmp_flux$dca <- tmp_flux$group
        background <- plot_dca_background(tmp_polys, tmp_labels)
        legend_flux='Max. Daily Flux\n(g/cm^2/day)'
         p1 <- plot_csc_site_nolabel(background, tmp_flux, i, 
                                    legend_title=legend_flux, 
                                    value_index=2, 
                                    value_max=1,
                                    plot_title="Daily Flux")
        fl <- tempfile()
        png(filename=fl, width=8, height=8, units="in", res=300)
        print(p1)
        dev.off()
        flux_plot <- png::readPNG(fl)
        flux_grobs[[i]] <- grid::rasterGrob(flux_plot, interpolate=TRUE)
    }
