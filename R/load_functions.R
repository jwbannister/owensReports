load_sandflux <- function(area, start_date, end_date){
    csc_list <- list("brine"=seq(1700, 1799, 1), 
                     "channel"=c(seq(1300, 1399, 1), seq(1100, 1199, 1)),
                     "dwm"=seq(1900, 1999, 1), 
                     "twb2"=seq(1600, 1699, 1))
    if (start_date < "2016-08-01"){
           csc_list$sfwcrft <- c(seq(1400, 1499, 1), seq(1500, 1599, 1)) 
    } else{
           csc_list$sfwcrft <- c(seq(1800, 1899, 1))
    }

    query1 <- paste0("SELECT flux.datetime, idep.deployment AS csc, ",
                     "flux.sensit, flux.sand_flux, flux.ws_10m, ",
                     "COALESCE(flux.wd_10m, flux.resultant_wd_10m) ",
                     "AS wd_10m, idep.northing_utm, idep.easting_utm, ",
                     "flux.invalid ",
                     "FROM sandcatch.sandflux_5min flux ",
                     "JOIN instruments.deployments idep ",
                     "ON flux.csc_deployment_id=idep.deployment_id ",
                     "WHERE flux.invalid='FALSE' ",
                     "AND datetime::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date ",
                     "AND idep.deployment ",
                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
    flux_df <- owensData::query_owens_aws(query1)
    flux_df
}
