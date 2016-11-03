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

background <- plot_dca_background("T3SW", owens_areas$dca$polygons, 
                                  owens_areas$dca$labels)
p1 <- plot_csc_masses(background, csc_mass, "T3SW")






  contour_plots <- vector(mode="list", length=length(areas))
  names(contour_plots) <- areas
  wind_plots <- contour_plots
  wind_data <- contour_plots
  for (i in areas){
    p1 <- plot_dcm_background(i, sfwct_polys, sfwct_labels)
    contour_plots[[i]] <- plot_csc_masses(p1, csc_mass, i)
    wind_plots[[i]] <- precollection_wind(i)
    wind_data[[i]] <- precollection_wind(i)$data
  }

for (i in 1:nrow(flux_summary)){
  dat <- filter(wind_data[[flux_summary$area[i]]],
                as.Date(ymd(substring(datetime, 1, 10)))==flux_summary$day[i])
  max_wind <- ifelse(nrow(dat)>0, max(dat$windspeed_10m), NA)
  flux_summary$max.windspeed[i] <- ifelse(nrow(dat)>0, round(max_wind, 1), NA)
  wind_dir <- arrange(dat, desc(windspeed_10m)) %>% 
    filter(!is.na(winddirection_10m))
  flux_summary$winddirection[i] <- ifelse(nrow(wind_dir)>0, 
                                          wind_dir$winddirection_10m[1],
                                          NA)
}

