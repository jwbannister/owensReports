load_sandflux <- function(area, start_date, end_date){
    devtools::load_all("~/code/owensData")
    csc_list <- list("brine"=seq(1701, 1710, 1), 
                     "channel"=c(seq(1301, 1314, 1), seq(1101, 1118, 1)),
                     "dwm"=seq(1901, 1929, 1), 
                     "sfwcrft"=c(seq(1420, 1488, 1), seq(1500, 1596, 1)),
                     "twb2"=seq(1600, 1640, 1))
    query1 <- paste0("SELECT flux.datetime, idep.deployment AS csc, ",
                     "flux.sensit, flux.sand_flux, flux.windspeed_10m, ",
                     "COALESCE(flux.winddirection_10m, flux.resultant_wd_10m) ",
                     "AS wd_10m, idep.northing_utm, idep.easting_utm, ",
                     "flux.invalid ",
                     "FROM sandcatches.sandflux_5min flux ",
                     "JOIN instruments.deployments idep ",
                     "ON flux.csc_deployment_id=idep.deployment_id ",
                     "WHERE datetime::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date ",
                     "AND idep.deployment ",
                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
    flux_df <- query_owenslake(query1)
    flux_df
}

load_sandflux_aws <- function(area, start_date, end_date){
    devtools::load_all("~/code/owensData")
    csc_list <- list("brine"=seq(1701, 1710, 1), 
                     "channel"=c(seq(1301, 1314, 1), seq(1101, 1118, 1)),
                     "dwm"=seq(1901, 1929, 1), 
                     "sfwcrft"=c(seq(1420, 1488, 1), seq(1500, 1596, 1)),
                     "twb2"=seq(1600, 1640, 1))
    query1 <- paste0("SELECT flux.datetime, idep.deployment AS csc, ",
                     "flux.sensit, flux.sand_flux, flux.ws_10m, ",
                     "COALESCE(flux.wd_10m, flux.resultant_wd_10m) ",
                     "AS wd_10m, idep.northing_utm, idep.easting_utm, ",
                     "flux.invalid ",
                     "FROM sandcatch.sandflux_5min_test flux ",
                     "JOIN instruments.deployments idep ",
                     "ON flux.csc_deployment_id=idep.deployment_id ",
                     "WHERE datetime::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date ",
                     "AND idep.deployment ",
                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
    flux_df <- query_owens_aws(query1)
    flux_df
}
