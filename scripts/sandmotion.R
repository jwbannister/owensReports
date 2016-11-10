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
    csc_plots <- vector(mode="list", length=length(unique(csc_mass$dca)))
    names(csc_plots) <- unique(csc_mass$dca)
    for (i in unique(csc_mass$dca)){
        print(i)
        tmp_polys <- filter(sfwcrft_areas$polygons, dca==i)
        tmp_labels <- filter(sfwcrft_areas$labels, dca==i)
        background <- plot_dca_background(unique(tmp_polys$area), 
                                          tmp_polys, tmp_labels)
        csc_plots[[i]] <- plot_csc_site_nolabel(background, csc_mass, i)
    }
    mass_ce <- summarize_sandmass(csc_mass)
}
    
csc_plots <- vector(mode="list", length=length(unique(csc_mass$dca)))
names(csc_plots) <- unique(csc_mass$dca)
for (i in unique(csc_mass$dca)){
    print(i)
    background <- plot_dca_background(i, owens_areas$dca$polygons, 
                                      owens_areas$dca$labels)
    csc_plots[[i]] <- plot_csc_site(background, csc_mass, i)
}




