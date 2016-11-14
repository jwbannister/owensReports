load_all()
load_all("~/code/owensMaps")
library(tidyverse)
library(lubridate)

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- flux_df %>% group_by(csc) %>% 
  summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1), 
            x=unique(easting_utm), y=unique(northing_utm)) %>% ungroup()
csc_mass$dca <- apply(cbind(csc_mass$x, csc_mass$y), 1, 
                      point_in_dca, poly_df=owens_areas$dca$polygons)

daily_flux <- flux_df %>% 
  group_by(csc, day=date(datetime)) %>% 
  summarize(sand.flux=round(sum(sand_flux), 2)) %>% ungroup() 
expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                            day=seq(start_date, end_date, "days"), 
                            stringsAsFactors=FALSE)
full_daily <- left_join(expand_daily, daily_flux, by=c("csc", "day"))
full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0

max_daily <- full_daily %>% group_by(csc) %>%
    summarize(max.daily.flux = max(sand.flux)) 

csc_mass <- left_join(csc_mass, max_daily, by="csc")

if (area=="sfwcrft"){
    sfwcrft_areas <- build_sfwcrft_areas()
    csc_mass$treatment <- apply(cbind(csc_mass$x, csc_mass$y), 1, 
                                point_in_dca, poly_df=sfwcrft_areas$polygons)
    sfwcrft_grobs <- vector(mode="list", length=length(unique(csc_mass$dca)))
    names(sfwcrft_grobs) <- unique(csc_mass$dca)
    for (i in unique(csc_mass$dca)){
        print(i)
        tmp_polys <- filter(sfwcrft_areas$polygons, dca==i)
        tmp_labels <- filter(sfwcrft_areas$labels, dca==i)
        background <- plot_dca_background(unique(tmp_polys$area), 
                                          tmp_polys, tmp_labels)
        legend_mass='Total Mass (g)'
        fl1 <- tempfile()
        png(filename=fl1, width=6, height=6, units="in", res=300)
        print(plot_csc_site_nolabel(background, csc_mass, i, 
                              legend_title=legend_mass, 
                              value_index=2, value_max=200))
        dev.off()
        mass_plot <- png::readPNG(fl1)
        mass_grob <- grid::rasterGrob(mass_plot, interpolate=TRUE)

        legend_flux='Max. Daily Flux\n(g/cm^2/day)\n'
        max_flux <- max(filter(csc_mass, dca==i)$max.daily.flux)
        fl2 <- tempfile()
        png(filename=fl2, width=6, height=6, units="in", res=300)
        print(plot_csc_site_nolabel(background, csc_mass, i, 
                              legend_title=legend_flux, 
                              value_index=6, 
                              value_max=max_flux))
        dev.off()
        flux_plot <- png::readPNG(fl2)
        flux_grob <- grid::rasterGrob(flux_plot, interpolate=TRUE)

        fl3 <- tempfile()
        png(filename=fl3, width=8, height=4, units="in", res=300)
        gridExtra::grid.arrange(mass_grob, flux_grob, ncol=2)
        dev.off()
        combo_plot <- png::readPNG(fl3)
        sfwcrft_grobs[[i]] <- grid::rasterGrob(combo_plot, interpolate=TRUE)
    }
    mass_ce <- calc_mass_ce_sfwcrft(csc_mass)

    full_daily <- full_daily %>%
        left_join(dplyr::select(csc_mass, csc, dca, treatment), by="csc") 
    flux_ce <- calc_flux_ce_sfwcrft(full_daily)
}

if (area=="twb2"){
    csc_plots <- vector(mode="list", length=length(unique(csc_mass$dca)))
    names(csc_plots) <- unique(csc_mass$dca)
    for (i in unique(csc_mass$dca)){
        print(i)
        background <- plot_dca_background(i, owens_areas$dca$polygons, 
                                          owens_areas$dca$labels)
        csc_plots[[i]] <- plot_csc_site(background, csc_mass, i)
    }
}



