load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)

if (area=="dwm") owens$labels <- move_dwm_labels()
if (area=="brine") owens$labels <- move_brine_labels()
if (area=="channel") owens$labels <- move_channel_labels()
if (area=="t1a1") owens$labels <- move_t1a1_labels()

daily_flux <- flux_df %>% 
    group_by(csc, date=date(datetime)) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2), 
              x=unique(easting_utm), y=unique(northing_utm),
              bad_coll=any(bad_coll)) %>% ungroup() 
    expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                                date=seq(start_date, end_date, "days"), 
                                stringsAsFactors=FALSE)
    full_daily <- expand_daily %>%
        left_join(select(daily_flux, -x, -y), by=c("csc", "date")) %>%
        left_join(select(sites_df, csc, dca), by="csc")
    full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0

    prelim_max_daily <- full_daily %>% group_by(csc) %>%
        summarize(max.daily.flux = max(sand.flux))

    max_daily <- left_join(prelim_max_daily, select(sites_df, csc, x, y, dca),
                           by="csc")
    max_daily[is.na(max_daily$max.daily.flux), "max.daily.flux"] <- 0
    bad_collections <- daily_flux %>% group_by(csc) %>%
        summarize(x=unique(x), y=unique(y),
                  bad_count=sum(bad_coll), good_count=sum(!bad_coll)) %>%
    filter(bad_count>0) %>%
    left_join(select(sites_df, csc, dca), by="csc")
if (nrow(bad_collections)>0){
    bad_collections$flag <- sapply(bad_collections$good_count, function(x) 
                                   if_else(x==0, "No Data For Month", 
                                           "Partial Data For Month"))
    bad_collections$flag <- factor(bad_collections$flag)
}
# TEMPORARY FOR FEB BRINE REPORT
bad_collections <- daily_flux %>% group_by(csc) %>%
            summarize(x=unique(x), y=unique(y)) %>%
            filter(csc!='1705') %>%
    left_join(select(sites_df, csc, dca), by="csc")
    bad_collections$flag <- rep("No Data For Month", nrow(bad_collections))

    flux_grobs <- vector(mode="list", length=length(unique(max_daily$dca)))
    names(flux_grobs) <- unique(max_daily$dca)
    titles <- c("C1"="Channel Area North", "C2"="Channel Area South")
    for (i in unique(max_daily$dca)){
    tmp_bad <- filter(bad_collections, dca==i & flag=="No Data For Month")
    tmp_partial <- filter(bad_collections, 
                          dca==i & flag=="Partial Data For Month")
        print(i)
        tmp_polys <- filter(owens$polygons, dca==i) %>%
            select(x, y, id=objectid)
        if (area=="channel"){ 
            tmp_labels <- filter(owens$labels, dca==i) %>% 
                mutate(id=titles[[dca]]) %>%
                select(x, y, id) 
        } else{
            tmp_labels <- filter(owens$labels, dca==i) %>% 
                select(x, y, id=dca)
        }
        background <- plot_dca_background(tmp_polys, tmp_labels)
        legend_flux='Max. Daily Flux\n(g/cm^2/day)'
        max_flux <- round(max(filter(max_daily, dca==i)$max.daily.flux), 0)
        if (area %in% c("channel", "t1a1")){
            p1 <- plot_csc_site_label_nocolor(background, max_daily, i, 
                                              legend_title=legend_flux, 
                                              value_index=2, 
                                              value_max=5,
                                              plot_title="Daily Flux")
        } else{
            p1 <- plot_csc_site(background, max_daily, i, 
                                        legend_title=legend_flux, 
                                        value_index=2, 
                                        value_max=5,
                                        plot_title="Daily Flux") +
            geom_point(data=tmp_partial, mapping=aes(x=x, y=y, shape=flag), 
                       color="black", size=6) +
            scale_shape_manual(name=NULL, values=c(21)) +
            geom_point(data=tmp_bad, mapping=aes(x=x, y=y, size=flag), 
                       color="black") +
            scale_size_manual(name=NULL, values=c(4))
        }
        fl <- tempfile()
        png(filename=fl, width=8, height=8, units="in", res=300)
        print(p1)
        dev.off()
        flux_plot <- png::readPNG(fl)
        flux_grobs[[i]] <- grid::rasterGrob(flux_plot, interpolate=TRUE)
    }
