load_all()
load_all("~/code/owensData")

csc_list <- list("brine"=seq(1701, 1710, 1), 
                 "channel"=c(seq(1301, 1314, 1), seq(1101, 1118, 1)),
                 "dwm"=seq(1901, 1929, 1), 
                 "sfwcrft"=seq(1801, 1880, 1),
                 "twb2"=seq(1600, 1640, 1))

area <- "twb2" # c("brine", "channel", "dwm", "sfwcrft", "twb2")
start_date <- "2016-03-01"
end_date <- "2016-04-01"

query1 <- paste0("SELECT flux.datetime, idep.deployment AS csc, flux.sensit, 
                 flux.sand_flux, flux.windspeed_10m, 
                 COALESCE(flux.winddirection_10m, flux.resultant_wd_10m) AS 
                 wd_10m, idep.northing_utm, idep.easting_utm, flux.invalid  
                 FROM sandcatches.sandflux_5min flux
                 JOIN instruments.deployments idep
                 ON flux.csc_deployment_id=idep.deployment_id
                 WHERE datetime::date 
                 BETWEEN '", start_date, "'::date AND '", end_date, "'::date
                 AND idep.deployment IN ('", 
                 paste0(csc_list[[area]], collapse="', '"),  
                 "');")
flux_df <- query_owenslake(query1)
