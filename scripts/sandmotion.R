if (!"package:dustReport" %in% search()) load_all("~/analysis/dustReport")
if (!"package:Rowens" %in% search()) load_all("~/analysis/Rowens")
if (!"package:dplyr" %in% search()) library(dplyr)
if (!"package:reshape2" %in% search()) library(reshape2)
if (!"package:lubridate" %in% search()) library(lubridate)

if (!exists("start.date")) start.date <- mdy("11-01-201")
if (!exists("end.date")) end.date <- start.date %m+% months(1) %m-% days(1)
period.month <- month(start.date, label=T, abbr=F)
period.year <- year(start.date)

if (!exists("study")) study <- "sfwct"
ifelse(study=="twb2",
       descrip <- "Tillage with BACM Backup (TwB2)", 
       descrip <- "Shallow Flood Wetness Cover Refinement Field Test")

query1 <- paste0("SELECT flux.*, idep.deployment, idep.northing_utm, 
                 idep.easting_utm, ia.area, ia.description
                 FROM sandcatches.sandflux_5min flux
                 JOIN instruments.deployments idep
                 ON flux.csc_deployment_id=idep.deployment_id
                 JOIN instruments.areas ia
                 ON idep.area_id=ia.area_id
                 WHERE datetime::date 
                 BETWEEN '", start.date, "'::date AND '", end.date, "'::date
                 AND ia.description='", descrip, "'")
flux_data <- query_owenslake(query1)
flux_data <- filter(flux_data, !(sand_flux<0))
ifelse(study=="sfwct", 
       df1 <- clean_flux_sfwct(flux_data),
       df1 <- clean_flux_twb2(flux_data))
flux_df <- df1[!df1$invalid, ]
ifelse(study=="sfwct", 
       flux_df <- mutate(flux_df, dcm=paste0(area, "_", treatment)) %>%
        select(-area, -treatment), 
       flux_df <- rename(flux_df, dcm=area)) 

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- flux_df %>% group_by(csc) %>% 
  summarize(sand.mass=sum(sand_flux)*geom_adj, x=unique(x), y=unique(y)) %>%
  inner_join(select(flux_df, csc, dcm)[!duplicated(flux_df$csc), ], by="csc")
csc_mass$sand.mass <- round(csc_mass$sand.mass, 1)

daily_flux <- flux_df %>% 
  mutate(day = as.Date(ymd(substring(datetime, 1, 10)))) %>%
  group_by(csc, day) %>% 
  summarize(sand.flux=round(sum(sand_flux), 2), dcm=unique(dcm)) %>% ungroup()
expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                            day=unique(daily_flux$day), 
                            stringsAsFactors=FALSE)
expand_daily <- left_join(expand_daily, select(daily_flux, csc, dcm), 
                           by="csc") %>% arrange(csc, day)
expand_daily <- expand_daily[!duplicated(expand_daily), ]
full_daily <- left_join(expand_daily, daily_flux, by=c("csc", "day", "dcm"))
full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0
daily_flux <- full_daily

max_daily_flux <- daily_flux %>% group_by(csc) %>%
  summarize(max.daily.flux = round(max(sand.flux), 2))

ifelse(study=="sfwct", 
       flux_summary <- summarize_flux_sfwct(daily_flux), 
       flux_summary <- summarize_flux_twb2(csc_mass, max_daily_flux))

if (study=="sfwct") {
  mass_summary <- summarize_sandmass(csc_mass)
  mass_summary$clean.eff <- clean_ce(mass_summary$control.eff)
  areas <- unique(mass_summary$area)
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
}

if (study=="sfwct"){
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
}

